package dev.rusk.bytecode;

import java.util.List;
import java.util.Objects;

public record Function(java.lang.String name, int regCount, int paramCount, List<Instruction> code) {
    public Function {
        Objects.requireNonNull(name, "name");
        Objects.requireNonNull(code, "code");
        code = List.copyOf(code);
        if (regCount < 0) {
            throw new IllegalArgumentException("regCount must be >= 0");
        }
        if (paramCount < 0) {
            throw new IllegalArgumentException("paramCount must be >= 0");
        }
    }
}

