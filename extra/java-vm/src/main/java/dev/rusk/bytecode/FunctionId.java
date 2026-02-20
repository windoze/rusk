package dev.rusk.bytecode;

public record FunctionId(int index) {
    public FunctionId {
        if (index < 0) {
            throw new IllegalArgumentException("FunctionId must be non-negative");
        }
    }
}

