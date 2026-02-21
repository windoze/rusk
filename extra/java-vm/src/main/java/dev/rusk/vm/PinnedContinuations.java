package dev.rusk.vm;

import java.util.ArrayList;
import java.util.Objects;

/**
 * 宿主可持有的 continuation（{@link AbiValue.Continuation}）的固定表。
 *
 * <p>对齐 Rust 参考实现：
 *
 * <ul>
 *   <li>{@code ContinuationHandle.index == 0} 保留给 {@link StepResult.Request} 的挂起句柄；
 *   <li>宿主可存储的 continuation 句柄使用 {@code index >= 1}；
 *   <li>handle 带 generation，用于使旧句柄失效；
 *   <li>continuation token 是 one-shot：被 VM 消耗后，旧 handle 仍然存在但不可再 resolve。
 * </ul>
 */
final class PinnedContinuations {
    private static final class Slot {
        int generation;
        ContinuationToken token; // null 表示已 drop
    }

    private final ArrayList<Slot> slots = new ArrayList<>();
    private final ArrayList<Integer> free = new ArrayList<>();

    ContinuationHandle pin(ContinuationToken token) {
        Objects.requireNonNull(token, "token");

        int slotIndex;
        if (!free.isEmpty()) {
            slotIndex = free.remove(free.size() - 1);
        } else {
            slotIndex = slots.size();
            slots.add(new Slot());
        }

        if (slotIndex < 0 || slotIndex >= slots.size()) {
            throw new IllegalStateException("internal error: pinned continuation slot out of range");
        }

        Slot slot = slots.get(slotIndex);
        slot.token = token;

        long handleIndex = (long) slotIndex + 1L; // reserve 0 for step API
        if (handleIndex > Integer.MAX_VALUE) {
            throw new IllegalStateException("pinned continuation handle overflow (too many continuations)");
        }
        return new ContinuationHandle((int) handleIndex, slot.generation);
    }

    ContinuationToken resolve(ContinuationHandle handle) throws VmError.InvalidContinuation {
        Objects.requireNonNull(handle, "handle");

        if (handle.index() == 0) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: index 0 is reserved");
        }
        int slotIndex = handle.index() - 1;
        if (slotIndex < 0 || slotIndex >= slots.size()) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: out of range");
        }
        Slot slot = slots.get(slotIndex);
        if (slot.generation != handle.generation()) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: generation mismatch");
        }
        if (slot.token == null) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: dropped");
        }
        if (slot.token.isConsumed()) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: already consumed");
        }
        return slot.token;
    }

    void dropPinned(ContinuationHandle handle) throws VmError.InvalidContinuation {
        Objects.requireNonNull(handle, "handle");

        if (handle.index() == 0) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: index 0 is reserved");
        }
        int slotIndex = handle.index() - 1;
        if (slotIndex < 0 || slotIndex >= slots.size()) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: out of range");
        }
        Slot slot = slots.get(slotIndex);
        if (slot.generation != handle.generation()) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: generation mismatch");
        }
        if (slot.token == null) {
            throw new VmError.InvalidContinuation("invalid pinned continuation handle: already dropped");
        }

        slot.token = null;
        slot.generation = slot.generation + 1;
        free.add(slotIndex);
    }
}

