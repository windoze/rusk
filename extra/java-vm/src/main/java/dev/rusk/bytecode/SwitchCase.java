package dev.rusk.bytecode;

import java.util.List;
import java.util.Objects;

public record SwitchCase(Pattern pattern, int targetPc, List<Integer> paramRegs) {
    public SwitchCase {
        Objects.requireNonNull(pattern, "pattern");
        Objects.requireNonNull(paramRegs, "paramRegs");
        paramRegs = List.copyOf(paramRegs);
    }
}

