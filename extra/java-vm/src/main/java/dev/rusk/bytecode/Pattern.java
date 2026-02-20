package dev.rusk.bytecode;

import java.util.List;
import java.util.Objects;

public sealed interface Pattern
        permits Pattern.Wildcard,
                Pattern.Bind,
                Pattern.Literal,
                Pattern.Tuple,
                Pattern.Enum,
                Pattern.Struct,
                Pattern.Array {
    record Wildcard() implements Pattern {}

    record Bind() implements Pattern {}

    record Literal(ConstValue value) implements Pattern {
        public Literal {
            Objects.requireNonNull(value, "value");
        }
    }

    /**
     * {@code (prefix..., ..rest, suffix...)}.
     * <p>
     * 若 {@code rest == null} 表示无 rest（长度必须精确匹配）。
     */
    record Tuple(List<Pattern> prefix, Pattern rest, List<Pattern> suffix) implements Pattern {
        public Tuple {
            Objects.requireNonNull(prefix, "prefix");
            Objects.requireNonNull(suffix, "suffix");
            prefix = List.copyOf(prefix);
            suffix = List.copyOf(suffix);
        }
    }

    record Enum(java.lang.String enumName, java.lang.String variant, List<Pattern> fields)
            implements Pattern {
        public Enum {
            Objects.requireNonNull(enumName, "enumName");
            Objects.requireNonNull(variant, "variant");
            Objects.requireNonNull(fields, "fields");
            fields = List.copyOf(fields);
        }
    }

    record Struct(java.lang.String typeName, List<Field> fields) implements Pattern {
        public Struct {
            Objects.requireNonNull(typeName, "typeName");
            Objects.requireNonNull(fields, "fields");
            fields = List.copyOf(fields);
        }

        public record Field(java.lang.String name, Pattern pattern) {
            public Field {
                Objects.requireNonNull(name, "name");
                Objects.requireNonNull(pattern, "pattern");
            }
        }
    }

    /**
     * {@code [prefix..., ..rest, suffix...]}.
     * <p>
     * 若 {@code rest == null} 表示无 rest（长度必须精确匹配）。
     */
    record Array(List<Pattern> prefix, Pattern rest, List<Pattern> suffix) implements Pattern {
        public Array {
            Objects.requireNonNull(prefix, "prefix");
            Objects.requireNonNull(suffix, "suffix");
            prefix = List.copyOf(prefix);
            suffix = List.copyOf(suffix);
        }
    }
}

