package dev.rusk.bytecode;

import java.util.Arrays;
import java.util.Objects;

public sealed interface ConstValue
        permits ConstValue.Unit,
                ConstValue.Bool,
                ConstValue.Int,
                ConstValue.Float,
                ConstValue.Str,
                ConstValue.Bytes,
                ConstValue.TypeRep,
                ConstValue.Function {
    record Unit() implements ConstValue {}

    record Bool(boolean value) implements ConstValue {}

    record Int(long value) implements ConstValue {}

    record Float(double value) implements ConstValue {}

    record Str(java.lang.String value) implements ConstValue {
        public Str {
            Objects.requireNonNull(value, "value");
        }
    }

    record Bytes(byte[] value) implements ConstValue {
        public Bytes {
            Objects.requireNonNull(value, "value");
            value = Arrays.copyOf(value, value.length);
        }
    }

    record TypeRep(TypeRepLit value) implements ConstValue {
        public TypeRep {
            Objects.requireNonNull(value, "value");
        }
    }

    record Function(FunctionId value) implements ConstValue {
        public Function {
            Objects.requireNonNull(value, "value");
        }
    }
}

