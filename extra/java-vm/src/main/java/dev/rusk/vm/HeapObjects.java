package dev.rusk.vm;

import dev.rusk.bytecode.TypeId;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

sealed interface HeapObject permits StructObj, EnumObj, ArrayObj, TupleObj {}

final class StructObj implements HeapObject {
    final TypeId typeId;
    final List<TypeReps.TypeRepId> typeArgs;
    final ArrayList<Value> fields;

    StructObj(TypeId typeId, List<TypeReps.TypeRepId> typeArgs, ArrayList<Value> fields) {
        this.typeId = Objects.requireNonNull(typeId, "typeId");
        this.typeArgs = List.copyOf(Objects.requireNonNull(typeArgs, "typeArgs"));
        this.fields = Objects.requireNonNull(fields, "fields");
    }
}

final class EnumObj implements HeapObject {
    final TypeId typeId;
    final List<TypeReps.TypeRepId> typeArgs;
    final String variant;
    final ArrayList<Value> fields;

    EnumObj(TypeId typeId, List<TypeReps.TypeRepId> typeArgs, String variant, ArrayList<Value> fields) {
        this.typeId = Objects.requireNonNull(typeId, "typeId");
        this.typeArgs = List.copyOf(Objects.requireNonNull(typeArgs, "typeArgs"));
        this.variant = Objects.requireNonNull(variant, "variant");
        this.fields = Objects.requireNonNull(fields, "fields");
    }
}

final class ArrayObj implements HeapObject {
    final ArrayList<Value> items;

    ArrayObj(ArrayList<Value> items) {
        this.items = Objects.requireNonNull(items, "items");
    }
}

final class TupleObj implements HeapObject {
    final ArrayList<Value> items;

    TupleObj(ArrayList<Value> items) {
        this.items = Objects.requireNonNull(items, "items");
    }
}
