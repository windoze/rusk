package dev.rusk.vm;

import dev.rusk.bytecode.ExecutableModule;
import dev.rusk.bytecode.TypeId;
import dev.rusk.bytecode.TypeRepLit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

final class TypeReps {
    record TypeRepId(int index) {
        TypeRepId {
            if (index < 0) {
                throw new IllegalArgumentException("TypeRepId must be non-negative");
            }
        }
    }

    sealed interface TypeCtor
            permits TypeCtor.Unit,
                    TypeCtor.Never,
                    TypeCtor.Bool,
                    TypeCtor.Int,
                    TypeCtor.Float,
                    TypeCtor.Byte,
                    TypeCtor.Char,
                    TypeCtor.String,
                    TypeCtor.Bytes,
                    TypeCtor.Array,
                    TypeCtor.Tuple,
                    TypeCtor.Struct,
                    TypeCtor.Enum,
                    TypeCtor.Interface,
                    TypeCtor.Fn,
                    TypeCtor.Cont {
        record Unit() implements TypeCtor {}

        record Never() implements TypeCtor {}

        record Bool() implements TypeCtor {}

        record Int() implements TypeCtor {}

        record Float() implements TypeCtor {}

        record Byte() implements TypeCtor {}

        record Char() implements TypeCtor {}

        record String() implements TypeCtor {}

        record Bytes() implements TypeCtor {}

        record Array() implements TypeCtor {}

        record Tuple(int arity) implements TypeCtor {
            public Tuple {
                if (arity < 0) {
                    throw new IllegalArgumentException("tuple arity must be >= 0");
                }
            }
        }

        record Struct(TypeId typeId) implements TypeCtor {
            public Struct {
                Objects.requireNonNull(typeId, "typeId");
            }
        }

        record Enum(TypeId typeId) implements TypeCtor {
            public Enum {
                Objects.requireNonNull(typeId, "typeId");
            }
        }

        record Interface(TypeId typeId) implements TypeCtor {
            public Interface {
                Objects.requireNonNull(typeId, "typeId");
            }
        }

        record Fn() implements TypeCtor {}

        record Cont() implements TypeCtor {}
    }

    record TypeRepNode(TypeCtor ctor, List<TypeRepId> args) {
        TypeRepNode {
            Objects.requireNonNull(ctor, "ctor");
            Objects.requireNonNull(args, "args");
            args = List.copyOf(args);
        }
    }

    private final List<TypeRepNode> nodes = new ArrayList<>();
    private final Map<TypeRepNode, TypeRepId> interner = new HashMap<>();

    TypeRepId intern(TypeRepNode node) {
        TypeRepId existing = interner.get(node);
        if (existing != null) {
            return existing;
        }
        TypeRepId id = new TypeRepId(nodes.size());
        nodes.add(node);
        interner.put(node, id);
        return id;
    }

    TypeRepNode node(TypeRepId id) {
        int idx = id.index();
        if (idx < 0 || idx >= nodes.size()) {
            return null;
        }
        return nodes.get(idx);
    }

    static TypeCtor ctorFromLit(ExecutableModule module, TypeRepLit lit) {
        Objects.requireNonNull(module, "module");
        if (lit instanceof TypeRepLit.Unit) {
            return new TypeCtor.Unit();
        }
        if (lit instanceof TypeRepLit.Never) {
            return new TypeCtor.Never();
        }
        if (lit instanceof TypeRepLit.Bool) {
            return new TypeCtor.Bool();
        }
        if (lit instanceof TypeRepLit.Int) {
            return new TypeCtor.Int();
        }
        if (lit instanceof TypeRepLit.Float) {
            return new TypeCtor.Float();
        }
        if (lit instanceof TypeRepLit.Byte) {
            return new TypeCtor.Byte();
        }
        if (lit instanceof TypeRepLit.Char) {
            return new TypeCtor.Char();
        }
        if (lit instanceof TypeRepLit.String) {
            return new TypeCtor.String();
        }
        if (lit instanceof TypeRepLit.Bytes) {
            return new TypeCtor.Bytes();
        }
        if (lit instanceof TypeRepLit.Array) {
            return new TypeCtor.Array();
        }
        if (lit instanceof TypeRepLit.Tuple t) {
            return new TypeCtor.Tuple(t.arity());
        }
        if (lit instanceof TypeRepLit.Struct s) {
            TypeId typeId =
                    module.typeId(s.name())
                            .orElseThrow(() -> new IllegalArgumentException("unknown struct type `" + s.name() + "`"));
            return new TypeCtor.Struct(typeId);
        }
        if (lit instanceof TypeRepLit.Enum e) {
            TypeId typeId =
                    module.typeId(e.name())
                            .orElseThrow(() -> new IllegalArgumentException("unknown enum type `" + e.name() + "`"));
            return new TypeCtor.Enum(typeId);
        }
        if (lit instanceof TypeRepLit.Interface i) {
            TypeId typeId =
                    module.typeId(i.name())
                            .orElseThrow(
                                    () -> new IllegalArgumentException("unknown interface type `" + i.name() + "`"));
            return new TypeCtor.Interface(typeId);
        }
        if (lit instanceof TypeRepLit.Fn) {
            return new TypeCtor.Fn();
        }
        if (lit instanceof TypeRepLit.Cont) {
            return new TypeCtor.Cont();
        }
        throw new IllegalStateException("unknown TypeRepLit: " + lit);
    }
}
