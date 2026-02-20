package dev.rusk.vm;

import dev.rusk.bytecode.FunctionId;
import java.util.Objects;

sealed interface Value
        permits Value.Unit,
                Value.Bool,
                Value.Int,
                Value.Float,
                Value.Byte,
                Value.Char,
                Value.Str,
                Value.Bytes,
                Value.TypeRep,
                Value.Ref,
                Value.Function,
                Value.Continuation {
    String kind();

    default Value intoReadonlyView() {
        if (this instanceof Ref r) {
            return new Ref(r.value().asReadonly());
        }
        return this;
    }

    default AbiValue toAbiOrNull() {
        if (this instanceof Unit) {
            return new AbiValue.Unit();
        }
        if (this instanceof Bool b) {
            return new AbiValue.Bool(b.value());
        }
        if (this instanceof Int n) {
            return new AbiValue.Int(n.value());
        }
        if (this instanceof Float x) {
            return new AbiValue.Float(x.value());
        }
        if (this instanceof Str s) {
            return new AbiValue.Str(s.value().asJavaString());
        }
        if (this instanceof Bytes b) {
            return new AbiValue.Bytes(b.value().toByteArray());
        }
        return null;
    }

    static Value fromAbi(AbiValue v) {
        Objects.requireNonNull(v, "v");
        if (v instanceof AbiValue.Unit) {
            return new Unit();
        }
        if (v instanceof AbiValue.Bool b) {
            return new Bool(b.value());
        }
        if (v instanceof AbiValue.Int n) {
            return new Int(n.value());
        }
        if (v instanceof AbiValue.Float x) {
            return new Float(x.value());
        }
        if (v instanceof AbiValue.Str s) {
            return new Str(new RuskString(s.value()));
        }
        if (v instanceof AbiValue.Bytes b) {
            return new Bytes(new RuskBytes(b.value()));
        }
        throw new IllegalStateException("unknown AbiValue: " + v);
    }

    record Unit() implements Value {
        @Override
        public String kind() {
            return "unit";
        }
    }

    record Bool(boolean value) implements Value {
        @Override
        public String kind() {
            return "bool";
        }
    }

    record Int(long value) implements Value {
        @Override
        public String kind() {
            return "int";
        }
    }

    record Float(double value) implements Value {
        @Override
        public String kind() {
            return "float";
        }
    }

    record Byte(int value) implements Value {
        public Byte {
            if (value < 0 || value > 255) {
                throw new IllegalArgumentException("byte out of range: " + value);
            }
        }

        @Override
        public String kind() {
            return "byte";
        }
    }

    record Char(int codePoint) implements Value {
        public Char {
            if (!Character.isValidCodePoint(codePoint)
                    || (0xD800 <= codePoint && codePoint <= 0xDFFF)) {
                throw new IllegalArgumentException("invalid char code point: " + codePoint);
            }
        }

        @Override
        public String kind() {
            return "char";
        }
    }

    record Str(RuskString value) implements Value {
        public Str {
            Objects.requireNonNull(value, "value");
        }

        @Override
        public String kind() {
            return "string";
        }
    }

    record Bytes(RuskBytes value) implements Value {
        public Bytes {
            Objects.requireNonNull(value, "value");
        }

        @Override
        public String kind() {
            return "bytes";
        }
    }

    record TypeRep(TypeReps.TypeRepId value) implements Value {
        public TypeRep {
            Objects.requireNonNull(value, "value");
        }

        @Override
        public String kind() {
            return "typerep";
        }
    }

    record Ref(RefValue value) implements Value {
        public Ref {
            Objects.requireNonNull(value, "value");
        }

        @Override
        public String kind() {
            return "ref";
        }
    }

    record Function(FunctionId value) implements Value {
        public Function {
            Objects.requireNonNull(value, "value");
        }

        @Override
        public String kind() {
            return "function";
        }
    }

    record Continuation(ContinuationToken token) implements Value {
        public Continuation {
            Objects.requireNonNull(token, "token");
        }

        @Override
        public String kind() {
            return "continuation";
        }
    }

    record RefValue(HeapObject obj, boolean readonly) {
        public RefValue {
            Objects.requireNonNull(obj, "obj");
        }

        RefValue asReadonly() {
            if (readonly) {
                return this;
            }
            return new RefValue(obj, true);
        }
    }
}

