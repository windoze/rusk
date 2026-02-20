package dev.rusk.bytecode;

import java.util.List;
import java.util.Objects;

public record HandlerClause(
        EffectSpec effect,
        List<Pattern> argPatterns,
        int targetPc,
        List<Integer> paramRegs) {
    public HandlerClause {
        Objects.requireNonNull(effect, "effect");
        Objects.requireNonNull(argPatterns, "argPatterns");
        Objects.requireNonNull(paramRegs, "paramRegs");
        argPatterns = List.copyOf(argPatterns);
        paramRegs = List.copyOf(paramRegs);
    }
}

