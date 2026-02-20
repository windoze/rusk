package dev.rusk.vm;

public sealed class VmError extends Exception permits VmError.InvalidState, VmError.InvalidContinuation {
    VmError(String message) {
        super(message);
    }

    public static final class InvalidState extends VmError {
        public InvalidState(String message) {
            super(message);
        }
    }

    public static final class InvalidContinuation extends VmError {
        public InvalidContinuation(String message) {
            super(message);
        }
    }
}

