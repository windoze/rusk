use alloc::alloc::{Layout, alloc, dealloc, handle_alloc_error};
use alloc::vec::Vec;
use core::cell::Cell;
use core::cmp::Ordering;
use core::mem::ManuallyDrop;
use core::ptr::{self, NonNull};

use crate::{GcHeap, GcRef, Trace, Tracer};

const BLOCK_SIZE: usize = 32 * 1024;

const FREE_SLOT: u32 = u32::MAX;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
struct SegmentHeader {
    slot_index: u32,
    size: u32,
}

#[derive(Debug)]
struct Slot {
    generation: u32,
    marked_epoch: Cell<u32>,
    ptr: Cell<*mut u8>,
    pinned: bool,
}

#[derive(Debug)]
struct Block {
    ptr: NonNull<u8>,
    cursor: usize,
    has_holes: bool,
}

#[derive(Clone, Copy, Debug)]
struct ObjectLayout {
    segment_size: usize,
    payload_offset: usize,
    align: usize,
}

impl ObjectLayout {
    fn for_type<T>() -> Self {
        let header_layout = Layout::new::<SegmentHeader>();
        let payload_layout = Layout::new::<ManuallyDrop<T>>();
        let (layout, payload_offset) = header_layout
            .extend(payload_layout)
            .expect("object layout overflow");
        let layout = layout.pad_to_align();

        Self {
            segment_size: layout.size(),
            payload_offset,
            align: layout.align(),
        }
    }
}

/// Configuration for Immix selective defragmentation.
#[derive(Clone, Copy, Debug)]
pub struct DefragConfig {
    /// Enables evacuation-based defragmentation.
    pub enabled: bool,
    /// Minimum fragmentation ratio (`0.0..=1.0`) needed to consider a block.
    pub fragmentation_threshold: f32,
    /// Max blocks to evacuate per collection.
    pub max_blocks: usize,
    /// Max bytes to move per collection.
    pub max_bytes: usize,
}

impl Default for DefragConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            fragmentation_threshold: 0.35,
            max_blocks: 1,
            max_bytes: 512 * 1024,
        }
    }
}

/// Basic statistics for the last [`ImmixHeap::collect`] run.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ImmixStats {
    pub blocks: usize,
    pub live_objects: usize,
    pub freed_objects: usize,
    pub moved_objects: usize,
    pub moved_bytes: usize,
    pub reclaimed_blocks: usize,
}

/// A stop-the-world, single-threaded Immix-style heap using stable handles.
#[derive(Debug)]
pub struct ImmixHeap<T: Trace> {
    slots: Vec<Slot>,
    free: Vec<u32>,
    live: usize,
    epoch: u32,
    blocks: Vec<Block>,
    alloc_block: usize,
    object_layout: ObjectLayout,
    block_layout: Layout,
    defrag: DefragConfig,
    last_stats: ImmixStats,
    _phantom: core::marker::PhantomData<T>,
}

impl<T: Trace> Default for ImmixHeap<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Trace> ImmixHeap<T> {
    pub fn new() -> Self {
        let object_layout = ObjectLayout::for_type::<T>();
        let block_align = object_layout.align.max(16);
        let block_layout =
            Layout::from_size_align(BLOCK_SIZE, block_align).expect("invalid block layout");

        Self {
            slots: Vec::new(),
            free: Vec::new(),
            live: 0,
            epoch: 0,
            blocks: Vec::new(),
            alloc_block: 0,
            object_layout,
            block_layout,
            defrag: DefragConfig::default(),
            last_stats: ImmixStats::default(),
            _phantom: core::marker::PhantomData,
        }
    }

    pub fn defrag_config(&self) -> DefragConfig {
        self.defrag
    }

    pub fn set_defrag_config(&mut self, config: DefragConfig) {
        self.defrag = config;
    }

    pub fn stats(&self) -> ImmixStats {
        self.last_stats
    }

    pub fn alloc_pinned(&mut self, value: T) -> GcRef {
        let handle = <Self as GcHeap<T>>::alloc(self, value);
        self.slots[handle.index as usize].pinned = true;
        handle
    }

    fn slot(&self, handle: GcRef) -> Option<&Slot> {
        let slot = self.slots.get(handle.index as usize)?;
        if slot.generation != handle.generation {
            return None;
        }
        if slot.ptr.get().is_null() {
            return None;
        }
        Some(slot)
    }

    fn allocate_slot(&mut self) -> (u32, u32) {
        if let Some(index) = self.free.pop() {
            let slot = &mut self.slots[index as usize];
            debug_assert!(slot.ptr.get().is_null(), "free list points at live slot");
            slot.marked_epoch.set(0);
            slot.pinned = false;
            return (index, slot.generation);
        }

        let index: u32 = self.slots.len().try_into().expect("gc heap index overflow");
        self.slots.push(Slot {
            generation: 0,
            marked_epoch: Cell::new(0),
            ptr: Cell::new(ptr::null_mut()),
            pinned: false,
        });
        (index, 0)
    }

    fn allocate_block(&mut self) -> usize {
        let ptr = unsafe {
            let raw = alloc(self.block_layout);
            if raw.is_null() {
                handle_alloc_error(self.block_layout);
            }
            NonNull::new_unchecked(raw)
        };
        self.blocks.push(Block {
            ptr,
            cursor: 0,
            has_holes: false,
        });
        self.blocks.len() - 1
    }

    unsafe fn payload_ptr(&self, header_ptr: *mut SegmentHeader) -> *mut ManuallyDrop<T> {
        unsafe {
            header_ptr
                .cast::<u8>()
                .add(self.object_layout.payload_offset)
                .cast::<ManuallyDrop<T>>()
        }
    }

    unsafe fn payload_ref<'a>(&self, header_ptr: *mut SegmentHeader) -> &'a T {
        unsafe { &*self.payload_ptr(header_ptr).cast::<T>() }
    }

    unsafe fn payload_mut<'a>(&self, header_ptr: *mut SegmentHeader) -> &'a mut T {
        unsafe { &mut *self.payload_ptr(header_ptr).cast::<T>() }
    }

    fn find_hole_in_block(block: &Block, required_size: usize) -> Option<(usize, usize)> {
        let base = block.ptr.as_ptr();
        let mut offset = 0usize;
        while offset < block.cursor {
            let header_ptr = unsafe { base.add(offset).cast::<SegmentHeader>() };
            let header = unsafe { ptr::read(header_ptr) };
            let size = header.size as usize;
            if size == 0 || offset + size > block.cursor {
                return None;
            }
            if header.slot_index == FREE_SLOT && size >= required_size {
                return Some((offset, size));
            }
            offset += size;
        }
        None
    }

    fn alloc_segment(
        &mut self,
        required_size: usize,
        exclude_block: Option<usize>,
    ) -> (*mut SegmentHeader, usize) {
        debug_assert!(required_size.is_multiple_of(self.object_layout.align));

        if !self.blocks.is_empty() {
            if self.alloc_block >= self.blocks.len() {
                self.alloc_block = self.blocks.len().saturating_sub(1);
            }

            // Bump allocation fast path: try the current block first, then search others.
            let start = self.alloc_block;
            for block_index in start..self.blocks.len() {
                if exclude_block == Some(block_index) {
                    continue;
                }
                let can_alloc = {
                    let block = &self.blocks[block_index];
                    block.cursor + required_size <= BLOCK_SIZE
                };
                if !can_alloc {
                    continue;
                }
                let block = &mut self.blocks[block_index];
                let base = block.ptr.as_ptr();
                let header_ptr = unsafe { base.add(block.cursor).cast::<SegmentHeader>() };
                block.cursor += required_size;
                self.alloc_block = block_index;
                return (header_ptr, required_size);
            }

            for block_index in 0..start {
                if exclude_block == Some(block_index) {
                    continue;
                }
                let can_alloc = {
                    let block = &self.blocks[block_index];
                    block.cursor + required_size <= BLOCK_SIZE
                };
                if !can_alloc {
                    continue;
                }
                let block = &mut self.blocks[block_index];
                let base = block.ptr.as_ptr();
                let header_ptr = unsafe { base.add(block.cursor).cast::<SegmentHeader>() };
                block.cursor += required_size;
                self.alloc_block = block_index;
                return (header_ptr, required_size);
            }

            // Only fall back to hole-filling when bump allocation has failed across the heap.
            for block_index in 0..self.blocks.len() {
                if exclude_block == Some(block_index) {
                    continue;
                }
                if !self.blocks[block_index].has_holes {
                    continue;
                }

                let hole = {
                    let block = &self.blocks[block_index];
                    Self::find_hole_in_block(block, required_size)
                };
                let Some((offset, hole_size)) = hole else {
                    self.blocks[block_index].has_holes = false;
                    continue;
                };

                let base = self.blocks[block_index].ptr.as_ptr();
                let header_ptr = unsafe { base.add(offset).cast::<SegmentHeader>() };

                let remainder = hole_size.saturating_sub(required_size);
                let min_free_segment = align_up(
                    core::mem::size_of::<SegmentHeader>(),
                    self.object_layout.align,
                );
                let (alloc_size, remainder_size) = if remainder >= min_free_segment {
                    (required_size, remainder)
                } else {
                    (hole_size, 0)
                };

                if remainder_size != 0 {
                    let rem_ptr = unsafe { base.add(offset + alloc_size).cast::<SegmentHeader>() };
                    unsafe {
                        ptr::write(
                            rem_ptr,
                            SegmentHeader {
                                slot_index: FREE_SLOT,
                                size: remainder_size as u32,
                            },
                        );
                    }
                    self.blocks[block_index].has_holes = true;
                }

                return (header_ptr, alloc_size);
            }
        }

        let new_block = self.allocate_block();
        debug_assert!(exclude_block != Some(new_block));

        let block = &mut self.blocks[new_block];
        let header_ptr = block.ptr.as_ptr().cast::<SegmentHeader>();
        block.cursor = required_size;
        self.alloc_block = new_block;
        (header_ptr, required_size)
    }

    fn prepare_collection(&mut self) {
        self.epoch = self.epoch.wrapping_add(1);
        if self.epoch == 0 {
            for slot in &self.slots {
                slot.marked_epoch.set(0);
            }
            self.epoch = 1;
        }
        self.last_stats = ImmixStats {
            blocks: self.blocks.len(),
            live_objects: self.live,
            freed_objects: 0,
            moved_objects: 0,
            moved_bytes: 0,
            reclaimed_blocks: 0,
        };
    }

    fn sweep(&mut self) -> Vec<BlockMetric> {
        let mut freed_objects = 0usize;
        let mut metrics = Vec::with_capacity(self.blocks.len());

        for block_index in 0..self.blocks.len() {
            let (live_bytes, used_bytes) = self.sweep_block(block_index, &mut freed_objects);
            metrics.push(BlockMetric {
                index: block_index,
                live_bytes,
                used_bytes,
            });

            if live_bytes == 0 && used_bytes != 0 {
                // Fast reset: keep the block but make it empty.
                self.blocks[block_index].cursor = 0;
                self.blocks[block_index].has_holes = false;
            }
        }

        self.last_stats.freed_objects = freed_objects;
        self.last_stats.live_objects = self.live;
        metrics
    }

    fn sweep_block(&mut self, block_index: usize, freed_objects: &mut usize) -> (usize, usize) {
        self.blocks[block_index].has_holes = false;
        let base = self.blocks[block_index].ptr.as_ptr();
        let used_bytes = self.blocks[block_index].cursor;
        let mut has_holes = false;
        let mut offset = 0usize;
        let mut live_bytes = 0usize;

        while offset < used_bytes {
            let header_ptr = unsafe { base.add(offset).cast::<SegmentHeader>() };
            let header = unsafe { ptr::read(header_ptr) };
            let size = header.size as usize;
            if size == 0 || offset + size > used_bytes {
                break;
            }

            if header.slot_index == FREE_SLOT {
                offset += size;
                continue;
            }

            let slot_index = header.slot_index as usize;
            if slot_index >= self.slots.len() {
                unsafe {
                    (*header_ptr).slot_index = FREE_SLOT;
                }
                has_holes = true;
                offset += size;
                continue;
            }

            let slot_ptr = self.slots[slot_index].ptr.get();
            if slot_ptr != header_ptr.cast::<u8>() {
                unsafe {
                    (*header_ptr).slot_index = FREE_SLOT;
                }
                has_holes = true;
                offset += size;
                continue;
            }

            let marked = self.slots[slot_index].marked_epoch.get() == self.epoch;
            if marked {
                live_bytes += size;
                offset += size;
                continue;
            }

            unsafe {
                ptr::drop_in_place(self.payload_ptr(header_ptr).cast::<T>());
                (*header_ptr).slot_index = FREE_SLOT;
            }
            has_holes = true;

            self.slots[slot_index].ptr.set(ptr::null_mut());
            self.slots[slot_index].generation = self.slots[slot_index].generation.wrapping_add(1);
            self.free.push(slot_index as u32);
            self.live = self.live.saturating_sub(1);
            *freed_objects += 1;

            offset += size;
        }

        self.blocks[block_index].has_holes = has_holes;
        (live_bytes, used_bytes)
    }

    fn defrag(&mut self, metrics: Vec<BlockMetric>) {
        if !self.defrag.enabled || self.defrag.max_blocks == 0 || self.defrag.max_bytes == 0 {
            return;
        }

        let mut candidates: Vec<BlockCandidate> = metrics
            .into_iter()
            .filter_map(|m| {
                if m.used_bytes == 0 || m.live_bytes == 0 {
                    return None;
                }
                let frag = 1.0 - (m.live_bytes as f32 / m.used_bytes as f32);
                if frag < self.defrag.fragmentation_threshold {
                    return None;
                }
                Some(BlockCandidate {
                    index: m.index,
                    fragmentation: frag,
                })
            })
            .collect();

        candidates.sort_by(|a, b| {
            b.fragmentation
                .partial_cmp(&a.fragmentation)
                .unwrap_or(Ordering::Equal)
        });
        candidates.truncate(self.defrag.max_blocks);

        // Evacuate in descending index order to keep indices stable while removing blocks.
        let mut indices: Vec<usize> = candidates.into_iter().map(|c| c.index).collect();
        indices.sort_unstable_by(|a, b| b.cmp(a));

        let mut remaining_bytes = self.defrag.max_bytes;
        for block_index in indices {
            if remaining_bytes == 0 {
                break;
            }
            if block_index >= self.blocks.len() {
                continue;
            }
            let moved = self.evacuate_block(block_index, &mut remaining_bytes);
            if moved.is_some() {
                self.last_stats.reclaimed_blocks += 1;
            }
        }

        self.last_stats.blocks = self.blocks.len();
    }

    fn evacuate_block(&mut self, block_index: usize, remaining_bytes: &mut usize) -> Option<()> {
        let used_bytes = self.blocks[block_index].cursor;
        if used_bytes == 0 {
            return None;
        }

        let base = self.blocks[block_index].ptr.as_ptr();
        let mut offset = 0usize;
        let mut live_offsets = Vec::new();

        while offset < used_bytes {
            let header_ptr = unsafe { base.add(offset).cast::<SegmentHeader>() };
            let header = unsafe { ptr::read(header_ptr) };
            let size = header.size as usize;
            if size == 0 || offset + size > used_bytes {
                return None;
            }

            if header.slot_index == FREE_SLOT {
                offset += size;
                continue;
            }

            let slot_index = header.slot_index as usize;
            if slot_index >= self.slots.len() {
                offset += size;
                continue;
            }
            if self.slots[slot_index].ptr.get() != header_ptr.cast::<u8>() {
                offset += size;
                continue;
            }

            if self.slots[slot_index].pinned {
                return None;
            }

            let marked = self.slots[slot_index].marked_epoch.get() == self.epoch;
            if marked {
                live_offsets.push(offset);
            }
            offset += size;
        }

        let request = self.object_layout.segment_size;
        let needed_bytes = request.saturating_mul(live_offsets.len());
        let moved_objects = live_offsets.len();
        if needed_bytes == 0 || *remaining_bytes < needed_bytes {
            return None;
        }

        for offset in live_offsets {
            let header_ptr = unsafe { base.add(offset).cast::<SegmentHeader>() };
            let slot_index = unsafe { (*header_ptr).slot_index } as usize;
            debug_assert!(slot_index < self.slots.len());
            debug_assert!(self.slots[slot_index].ptr.get() == header_ptr.cast::<u8>());
            debug_assert!(self.slots[slot_index].marked_epoch.get() == self.epoch);

            let (new_header_ptr, new_seg_size) = self.alloc_segment(request, Some(block_index));
            unsafe {
                ptr::write(
                    new_header_ptr,
                    SegmentHeader {
                        slot_index: slot_index as u32,
                        size: new_seg_size as u32,
                    },
                );

                let value = ptr::read(self.payload_ptr(header_ptr));
                ptr::write(self.payload_ptr(new_header_ptr), value);
            }
            self.slots[slot_index].ptr.set(new_header_ptr.cast::<u8>());
        }

        self.last_stats.moved_objects += moved_objects;
        self.last_stats.moved_bytes += needed_bytes;
        *remaining_bytes -= needed_bytes;

        let last_index = self.blocks.len().saturating_sub(1);
        let block = self.blocks.swap_remove(block_index);
        if self.blocks.is_empty() {
            self.alloc_block = 0;
        } else if self.alloc_block == last_index && block_index < last_index {
            self.alloc_block = block_index;
        } else if self.alloc_block == last_index && block_index == last_index {
            self.alloc_block = self.alloc_block.min(self.blocks.len().saturating_sub(1));
        }
        unsafe {
            dealloc(block.ptr.as_ptr(), self.block_layout);
        }
        Some(())
    }
}

impl<T: Trace> GcHeap<T> for ImmixHeap<T> {
    fn alloc(&mut self, value: T) -> GcRef {
        let (slot_index, generation) = self.allocate_slot();
        let request = self.object_layout.segment_size;
        let (header_ptr, seg_size) = self.alloc_segment(request, None);

        unsafe {
            ptr::write(
                header_ptr,
                SegmentHeader {
                    slot_index,
                    size: seg_size as u32,
                },
            );
            ptr::write(self.payload_ptr(header_ptr), ManuallyDrop::new(value));
        }

        self.slots[slot_index as usize]
            .ptr
            .set(header_ptr.cast::<u8>());
        self.live += 1;
        GcRef::new(slot_index, generation)
    }

    fn get(&self, handle: GcRef) -> Option<&T> {
        let slot = self.slot(handle)?;
        let header_ptr = slot.ptr.get().cast::<SegmentHeader>();
        Some(unsafe { self.payload_ref(header_ptr) })
    }

    fn get_mut(&mut self, handle: GcRef) -> Option<&mut T> {
        let slot = self.slot(handle)?;
        let header_ptr = slot.ptr.get().cast::<SegmentHeader>();
        Some(unsafe { self.payload_mut(header_ptr) })
    }

    fn collect(&mut self, roots: &dyn Trace) {
        self.prepare_collection();

        // Mark phase (iterative).
        {
            let heap_view: &ImmixHeap<T> = &*self;
            let mut tracer = ImmixTracer::new(heap_view);

            roots.trace(&mut tracer);
            while let Some(handle) = tracer.pop_work() {
                let Some(value) = heap_view.get(handle) else {
                    continue;
                };
                value.trace(&mut tracer);
            }
        }

        let metrics = self.sweep();
        self.defrag(metrics);
    }

    fn live_objects(&self) -> usize {
        self.live
    }
}

impl<T: Trace> Drop for ImmixHeap<T> {
    fn drop(&mut self) {
        for slot in &self.slots {
            let ptr = slot.ptr.get();
            if ptr.is_null() {
                continue;
            }
            let header_ptr = ptr.cast::<SegmentHeader>();
            unsafe {
                ptr::drop_in_place(self.payload_ptr(header_ptr).cast::<T>());
            }
            slot.ptr.set(ptr::null_mut());
        }

        for block in self.blocks.drain(..) {
            unsafe {
                dealloc(block.ptr.as_ptr(), self.block_layout);
            }
        }
    }
}

struct ImmixTracer<'a, T: Trace> {
    heap: &'a ImmixHeap<T>,
    worklist: Vec<GcRef>,
}

impl<'a, T: Trace> ImmixTracer<'a, T> {
    fn new(heap: &'a ImmixHeap<T>) -> Self {
        Self {
            heap,
            worklist: Vec::new(),
        }
    }

    fn pop_work(&mut self) -> Option<GcRef> {
        self.worklist.pop()
    }
}

impl<T: Trace> Tracer for ImmixTracer<'_, T> {
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

#[derive(Clone, Copy, Debug)]
struct BlockMetric {
    index: usize,
    live_bytes: usize,
    used_bytes: usize,
}

#[derive(Clone, Copy, Debug)]
struct BlockCandidate {
    index: usize,
    fragmentation: f32,
}

fn align_up(size: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (size + (align - 1)) & !(align - 1)
}
