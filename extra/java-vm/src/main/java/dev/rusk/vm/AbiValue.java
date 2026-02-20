package dev.rusk.vm;

import dev.rusk.bytecode.AbiType;
import java.util.Arrays;
import java.util.Objects;

public sealed interface AbiValue
        permits AbiValue.Unit,
                AbiValue.Bool,
                AbiValue.Int,
                AbiValue.Float,
                AbiValue.Str,
                AbiValue.Bytes {
    AbiType ty();

    record Unit() implements AbiValue {
        @Override
        public AbiType ty() {
            return AbiType.UNIT;
        }
    }

    record Bool(boolean value) implements AbiValue {
        @Override
        public AbiType ty() {
            return AbiType.BOOL;
        }
    }

    record Int(long value) implements AbiValue {
        @Override
        public AbiType ty() {
            return AbiType.INT;
        }
    }

    record Float(double value) implements AbiValue {
        @Override
        public AbiType ty() {
            return AbiType.FLOAT;
        }
    }

    record Str(String value) implements AbiValue {
        public Str {
            Objects.requireNonNull(value, "value");
        }

        @Override
        public AbiType ty() {
            return AbiType.STRING;
        }
    }

    record Bytes(byte[] value) implements AbiValue {
        public Bytes {
            Objects.requireNonNull(value, "value");
            value = Arrays.copyOf(value, value.length);
        }

        @Override
        public byte[] value() {
            return Arrays.copyOf(value, value.length);
        }

        @Override
        public AbiType ty() {
            return AbiType.BYTES;
        }
    }
}

