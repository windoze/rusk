package dev.rusk.bytecode;

public record MethodId(int index) {
    public MethodId {
        if (index < 0) {
            throw new IllegalArgumentException("MethodId must be non-negative");
        }
    }
}

