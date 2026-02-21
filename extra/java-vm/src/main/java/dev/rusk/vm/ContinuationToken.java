package dev.rusk.vm;

import java.util.List;
import java.util.Objects;

/**
 * 语言内 continuation token：一次性（one-shot），由 `perform` 捕获并由字节码 `resume` 消耗。
 */
final class ContinuationToken {
    private ContinuationState state; // null 表示已被 take

    ContinuationToken(ContinuationState state) {
        this.state = Objects.requireNonNull(state, "state");
    }

    ContinuationState takeState() {
        ContinuationState s = state;
        state = null;
        return s;
    }

    boolean isConsumed() {
        return state == null;
    }

    record ContinuationState(List<Frame> frames, List<HandlerEntry> handlers, Integer performDst) {
        public ContinuationState {
            Objects.requireNonNull(frames, "frames");
            Objects.requireNonNull(handlers, "handlers");
            frames = List.copyOf(frames);
            handlers = List.copyOf(handlers);
        }
    }
}
