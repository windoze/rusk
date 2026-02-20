package dev.rusk.bytecode;

public record EffectId(int index) {
    public EffectId {
        if (index < 0) {
            throw new IllegalArgumentException("EffectId must be non-negative");
        }
    }
}

