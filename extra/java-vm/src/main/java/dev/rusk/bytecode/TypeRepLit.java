package dev.rusk.bytecode;

import java.util.Objects;

public sealed interface TypeRepLit
        permits TypeRepLit.Unit,
                TypeRepLit.Never,
                TypeRepLit.Bool,
                TypeRepLit.Int,
                TypeRepLit.Float,
                TypeRepLit.Byte,
                TypeRepLit.Char,
                TypeRepLit.String,
                TypeRepLit.Bytes,
                TypeRepLit.Array,
                TypeRepLit.Tuple,
                TypeRepLit.Struct,
                TypeRepLit.Enum,
                TypeRepLit.Interface,
                TypeRepLit.Fn,
                TypeRepLit.Cont {
    record Unit() implements TypeRepLit {}

    record Never() implements TypeRepLit {}

    record Bool() implements TypeRepLit {}

    record Int() implements TypeRepLit {}

    record Float() implements TypeRepLit {}

    record Byte() implements TypeRepLit {}

    record Char() implements TypeRepLit {}

    record String() implements TypeRepLit {}

    record Bytes() implements TypeRepLit {}

    record Array() implements TypeRepLit {}

    record Tuple(int arity) implements TypeRepLit {
        public Tuple {
            if (arity < 0) {
                throw new IllegalArgumentException("tuple arity must be >= 0");
            }
        }
    }

    record Struct(java.lang.String name) implements TypeRepLit {
        public Struct {
            Objects.requireNonNull(name, "name");
        }
    }

    record Enum(java.lang.String name) implements TypeRepLit {
        public Enum {
            Objects.requireNonNull(name, "name");
        }
    }

    record Interface(java.lang.String name) implements TypeRepLit {
        public Interface {
            Objects.requireNonNull(name, "name");
        }
    }

    record Fn() implements TypeRepLit {}

    record Cont() implements TypeRepLit {}
}
