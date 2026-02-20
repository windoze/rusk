package dev.rusk.bytecode;

public record TypeId(int index) {
    public TypeId {
        if (index < 0) {
            throw new IllegalArgumentException("TypeId must be non-negative");
        }
    }
}

