#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::vec::Vec;
use core::cell::Cell;

/// A handle to a GC-managed heap object.
///
/// The `(index, generation)` pair prevents use-after-free when a slot is reused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GcRef {
    pub index: u32,
    pub generation: u32,
}

impl GcRef {
    pub const fn new(index: u32, generation: u32) -> Self {
        Self { index, generation }
    }
}

/// A value that can be traced by the GC.
pub trait Trace {
    fn trace(&self, tracer: &mut dyn Tracer);
}

/// A tracer used by [`Trace`] implementations to mark reachable heap objects.
pub trait Tracer {
    fn mark(&mut self, handle: GcRef);
}

/// Abstract GC heap interface.
pub trait GcHeap<T: Trace> {
    /// Allocates a new heap object and returns a handle to it.
    fn alloc(&mut self, value: T) -> GcRef;

    /// Gets an immutable reference to a heap object.
    fn get(&self, handle: GcRef) -> Option<&T>;

    /// Gets a mutable reference to a heap object.
    fn get_mut(&mut self, handle: GcRef) -> Option<&mut T>;

    /// Runs a tracing collection using `roots` as the root set.
    fn collect(&mut self, roots: &dyn Trace);

    /// Returns the number of currently live heap objects.
    fn live_objects(&self) -> usize;
}

/// A simple, single-threaded mark-and-sweep GC.
#[derive(Debug)]
pub struct MarkSweepHeap<T> {
    slots: Vec<MarkSweepSlot<T>>,
    free: Vec<u32>,
    live: usize,
    epoch: u32,
}

impl<T> Default for MarkSweepHeap<T> {
    fn default() -> Self {
        Self {
            slots: Vec::new(),
            free: Vec::new(),
            live: 0,
            epoch: 0,
        }
    }
}

#[derive(Debug)]
struct MarkSweepSlot<T> {
    generation: u32,
    marked_epoch: Cell<u32>,
    value: Option<T>,
}

impl<T> MarkSweepHeap<T> {
    fn slot(&self, handle: GcRef) -> Option<&MarkSweepSlot<T>> {
        let slot = self.slots.get(handle.index as usize)?;
        if slot.generation != handle.generation {
            return None;
        }
        slot.value.as_ref()?;
        Some(slot)
    }

    fn slot_mut(&mut self, handle: GcRef) -> Option<&mut MarkSweepSlot<T>> {
        let slot = self.slots.get_mut(handle.index as usize)?;
        if slot.generation != handle.generation {
            return None;
        }
        slot.value.as_ref()?;
        Some(slot)
    }
}

impl<T: Trace> GcHeap<T> for MarkSweepHeap<T> {
    fn alloc(&mut self, value: T) -> GcRef {
        if let Some(index) = self.free.pop() {
            let slot = &mut self.slots[index as usize];
            debug_assert!(slot.value.is_none(), "free list points at live slot");
            slot.value = Some(value);
            slot.marked_epoch.set(0);
            self.live += 1;
            return GcRef::new(index, slot.generation);
        }

        let index: u32 = self.slots.len().try_into().expect("gc heap index overflow");
        self.slots.push(MarkSweepSlot {
            generation: 0,
            marked_epoch: Cell::new(0),
            value: Some(value),
        });
        self.live += 1;
        GcRef::new(index, 0)
    }

    fn get(&self, handle: GcRef) -> Option<&T> {
        self.slot(handle)?.value.as_ref()
    }

    fn get_mut(&mut self, handle: GcRef) -> Option<&mut T> {
        self.slot_mut(handle)?.value.as_mut()
    }

    fn collect(&mut self, roots: &dyn Trace) {
        // Bump the mark epoch. Slots are considered marked iff `marked_epoch == self.epoch`.
        self.epoch = self.epoch.wrapping_add(1);
        if self.epoch == 0 {
            // Extremely unlikely wraparound (2^32 GCs): fall back to a full clear.
            for slot in &self.slots {
                slot.marked_epoch.set(0);
            }
            self.epoch = 1;
        }

        // Mark phase (iterative).
        {
            let heap_view: &MarkSweepHeap<T> = &*self;
            let mut tracer = MarkSweepTracer::new(heap_view);

            roots.trace(&mut tracer);
            while let Some(handle) = tracer.pop_work() {
                let Some(value) = heap_view.get(handle) else {
                    continue;
                };
                value.trace(&mut tracer);
            }
        }

        // Sweep phase.
        for (idx, slot) in self.slots.iter_mut().enumerate() {
            if slot.value.is_none() {
                continue;
            }
            if slot.marked_epoch.get() == self.epoch {
                continue;
            }

            slot.value = None;
            slot.generation = slot.generation.wrapping_add(1);
            self.free.push(idx as u32);
            self.live -= 1;
        }
    }

    fn live_objects(&self) -> usize {
        self.live
    }
}

struct MarkSweepTracer<'a, T> {
    heap: &'a MarkSweepHeap<T>,
    worklist: Vec<GcRef>,
}

impl<'a, T> MarkSweepTracer<'a, T> {
    fn new(heap: &'a MarkSweepHeap<T>) -> Self {
        Self {
            heap,
            worklist: Vec::new(),
        }
    }

    fn pop_work(&mut self) -> Option<GcRef> {
        self.worklist.pop()
    }
}

impl<T> Tracer for MarkSweepTracer<'_, T> {
    fn mark(&mut self, handle: GcRef) {
        let Some(slot) = self.heap.slot(handle) else {
            return;
        };
        if slot.marked_epoch.get() == self.heap.epoch {
            return;
        }
        slot.marked_epoch.set(self.heap.epoch);
        self.worklist.push(handle);
    }
}

mod immix;

pub use immix::{DefragConfig, ImmixHeap, ImmixStats};

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;
    use std::rc::Rc;

    #[derive(Debug)]
    struct Node {
        id: i64,
        child: Option<GcRef>,
    }

    impl Trace for Node {
        fn trace(&self, tracer: &mut dyn Tracer) {
            if let Some(child) = self.child {
                tracer.mark(child);
            }
        }
    }

    #[derive(Debug)]
    struct Root(GcRef);

    impl Trace for Root {
        fn trace(&self, tracer: &mut dyn Tracer) {
            tracer.mark(self.0);
        }
    }

    #[derive(Debug)]
    struct Empty;

    impl Trace for Empty {
        fn trace(&self, _tracer: &mut dyn Tracer) {}
    }

    #[test]
    fn immix_handles_remain_valid_across_gc_cycles() {
        let mut heap = ImmixHeap::<Node>::new();
        let handle = heap.alloc(Node { id: 1, child: None });

        heap.collect(&Root(handle));
        assert_eq!(heap.live_objects(), 1);
        assert_eq!(heap.get(handle).unwrap().id, 1);

        heap.collect(&Root(handle));
        assert_eq!(heap.live_objects(), 1);
        assert_eq!(heap.get(handle).unwrap().id, 1);
    }

    #[test]
    fn immix_prevents_stale_handle_use_after_reuse() {
        let mut heap = ImmixHeap::<Node>::new();
        let old = heap.alloc(Node { id: 1, child: None });

        heap.collect(&Empty);
        assert_eq!(heap.live_objects(), 0);
        assert!(heap.get(old).is_none());

        let new = heap.alloc(Node { id: 2, child: None });
        assert_eq!(old.index, new.index, "expected slot reuse");
        assert_ne!(old.generation, new.generation, "expected generation bump");
        assert_eq!(heap.get(new).unwrap().id, 2);
        assert!(heap.get(old).is_none(), "stale handle must not resolve");
    }

    #[test]
    fn immix_traces_through_nested_heap_graphs() {
        let mut heap = ImmixHeap::<Node>::new();
        let child = heap.alloc(Node { id: 2, child: None });
        let parent = heap.alloc(Node {
            id: 1,
            child: Some(child),
        });

        heap.collect(&Root(parent));
        assert_eq!(heap.live_objects(), 2);
        assert_eq!(heap.get(parent).unwrap().id, 1);
        assert_eq!(heap.get(child).unwrap().id, 2);

        heap.collect(&Empty);
        assert_eq!(heap.live_objects(), 0);
        assert!(heap.get(parent).is_none());
        assert!(heap.get(child).is_none());
    }

    #[derive(Clone, Debug)]
    struct DropCounter(Rc<Cell<usize>>);

    impl Drop for DropCounter {
        fn drop(&mut self) {
            self.0.set(self.0.get().saturating_add(1));
        }
    }

    #[derive(Debug)]
    struct Droppy {
        counter: DropCounter,
    }

    impl Trace for Droppy {
        fn trace(&self, _tracer: &mut dyn Tracer) {
            let _ = &self.counter;
        }
    }

    #[test]
    fn immix_defrag_moves_without_dropping_live_object() {
        let mut heap = ImmixHeap::<Droppy>::new();
        heap.set_defrag_config(DefragConfig {
            enabled: true,
            fragmentation_threshold: 0.0,
            max_blocks: 8,
            max_bytes: 10 * 1024 * 1024,
        });

        let dead_drops = Rc::new(Cell::new(0));
        let live_drops = Rc::new(Cell::new(0));

        let dead_n = 5000;
        for _ in 0..dead_n {
            heap.alloc(Droppy {
                counter: DropCounter(Rc::clone(&dead_drops)),
            });
        }
        let live = heap.alloc(Droppy {
            counter: DropCounter(Rc::clone(&live_drops)),
        });

        heap.collect(&Root(live));

        assert_eq!(heap.live_objects(), 1);
        assert_eq!(dead_drops.get(), dead_n);
        assert_eq!(
            live_drops.get(),
            0,
            "live object must not be dropped during defrag"
        );

        let stats = heap.stats();
        assert!(
            stats.moved_objects > 0,
            "expected at least one moved object, got {stats:?}"
        );
        assert!(
            stats.reclaimed_blocks > 0,
            "expected at least one reclaimed block, got {stats:?}"
        );

        heap.collect(&Empty);
        assert_eq!(heap.live_objects(), 0);
        assert_eq!(live_drops.get(), 1, "live object dropped exactly once");
    }

    #[derive(Debug)]
    struct Pinned {
        id: i64,
    }

    impl Trace for Pinned {
        fn trace(&self, _tracer: &mut dyn Tracer) {
            let _ = self.id;
        }
    }

    #[test]
    fn immix_defrag_skips_pinned_objects() {
        let mut heap = ImmixHeap::<Pinned>::new();
        heap.set_defrag_config(DefragConfig {
            enabled: true,
            fragmentation_threshold: 0.0,
            max_blocks: 8,
            max_bytes: 10 * 1024 * 1024,
        });

        let live = heap.alloc_pinned(Pinned { id: 1 });
        for i in 0..5000 {
            heap.alloc(Pinned { id: i + 2 });
        }

        heap.collect(&Root(live));
        assert_eq!(heap.live_objects(), 1);
        assert_eq!(heap.get(live).unwrap().id, 1);

        let stats = heap.stats();
        assert_eq!(stats.moved_objects, 0, "pinned object prevents evacuation");
        assert_eq!(
            stats.reclaimed_blocks, 0,
            "pinned block must not be reclaimed"
        );
    }
}
