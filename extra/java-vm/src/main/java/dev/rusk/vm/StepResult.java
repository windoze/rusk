package dev.rusk.vm;

import dev.rusk.bytecode.EffectId;
import java.util.List;
import java.util.Objects;

public sealed interface StepResult permits StepResult.Done, StepResult.Trap, StepResult.Request, StepResult.Yield {
    record Done(AbiValue value) implements StepResult {
        public Done {
            Objects.requireNonNull(value, "value");
        }
    }

    record Trap(String message) implements StepResult {
        public Trap {
            Objects.requireNonNull(message, "message");
        }
    }

    record Request(EffectId effectId, List<AbiValue> args, ContinuationHandle k) implements StepResult {
        public Request {
            Objects.requireNonNull(effectId, "effectId");
            Objects.requireNonNull(args, "args");
            Objects.requireNonNull(k, "k");
            args = List.copyOf(args);
        }
    }

    record Yield(long remainingFuel) implements StepResult {}
}

