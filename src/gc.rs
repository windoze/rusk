use crate::interpreter::HeapValue;
use std::cell::Cell;

/// A handle to a GC-managed heap object.
///
/// The `(index, generation)` pair prevents use-after-free if a slot is reused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GcRef {
    pub(crate) index: u32,
    pub(crate) generation: u32,
}

impl GcRef {
    pub(crate) fn new(index: u32, generation: u32) -> Self {
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
///
/// The interpreter is generic over this trait so GC implementations can be swapped.
pub trait GcHeap {
    /// Allocates a new heap object and returns a handle to it.
    fn alloc(&mut self, value: HeapValue) -> GcRef;

    /// Gets an immutable reference to a heap object.
    fn get(&self, handle: GcRef) -> Option<&HeapValue>;

    /// Gets a mutable reference to a heap object.
    fn get_mut(&mut self, handle: GcRef) -> Option<&mut HeapValue>;

    /// Runs a tracing collection using `roots` as the root set.
    fn collect_garbage(&mut self, roots: &dyn Trace);

    /// Returns the number of currently live heap objects.
    fn live_objects(&self) -> usize;
}

/// A simple, single-threaded mark-and-sweep GC.
#[derive(Debug, Default)]
pub struct MarkSweepHeap {
    slots: Vec<Slot>,
    free: Vec<u32>,
    live: usize,
}

#[derive(Debug)]
struct Slot {
    generation: u32,
    marked: Cell<bool>,
    value: Option<HeapValue>,
}

impl MarkSweepHeap {
    fn slot(&self, handle: GcRef) -> Option<&Slot> {
        let slot = self.slots.get(handle.index as usize)?;
        if slot.generation != handle.generation {
            return None;
        }
        slot.value.as_ref()?;
        Some(slot)
    }

    fn slot_mut(&mut self, handle: GcRef) -> Option<&mut Slot> {
        let slot = self.slots.get_mut(handle.index as usize)?;
        if slot.generation != handle.generation {
            return None;
        }
        slot.value.as_ref()?;
        Some(slot)
    }
}

impl GcHeap for MarkSweepHeap {
    fn alloc(&mut self, value: HeapValue) -> GcRef {
        if let Some(index) = self.free.pop() {
            let slot = &mut self.slots[index as usize];
            debug_assert!(slot.value.is_none(), "free list points at live slot");
            slot.value = Some(value);
            slot.marked.set(false);
            self.live += 1;
            return GcRef::new(index, slot.generation);
        }

        let index: u32 = self.slots.len().try_into().expect("gc heap index overflow");
        self.slots.push(Slot {
            generation: 0,
            marked: Cell::new(false),
            value: Some(value),
        });
        self.live += 1;
        GcRef::new(index, 0)
    }

    fn get(&self, handle: GcRef) -> Option<&HeapValue> {
        self.slot(handle)?.value.as_ref()
    }

    fn get_mut(&mut self, handle: GcRef) -> Option<&mut HeapValue> {
        self.slot_mut(handle)?.value.as_mut()
    }

    fn collect_garbage(&mut self, roots: &dyn Trace) {
        // Clear marks first.
        for slot in &self.slots {
            slot.marked.set(false);
        }

        // Mark phase (iterative).
        {
            let heap_view: &MarkSweepHeap = &*self;
            let mut tracer = MarkTracer::new(heap_view);

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
            if slot.marked.get() {
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

struct MarkTracer<'a> {
    heap: &'a MarkSweepHeap,
    worklist: Vec<GcRef>,
}

impl<'a> MarkTracer<'a> {
    fn new(heap: &'a MarkSweepHeap) -> Self {
        Self {
            heap,
            worklist: Vec::new(),
        }
    }

    fn pop_work(&mut self) -> Option<GcRef> {
        self.worklist.pop()
    }
}

impl Tracer for MarkTracer<'_> {
    fn mark(&mut self, handle: GcRef) {
        let Some(slot) = self.heap.slot(handle) else {
            return;
        };
        if slot.marked.get() {
            return;
        }
        slot.marked.set(true);
        self.worklist.push(handle);
    }
}
