package dev.rusk.bytecode;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.NavigableMap;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeMap;

/**
 * 与 Rust 侧 {@code rusk_bytecode::ExecutableModule} 对齐的紧凑可执行模块（Java 版）。
 *
 * <p>本类型同时维护若干派生索引表（name -> id 等）。为了保持一致性，建议通过本类的
 * {@code add_*} 方法构建，而不要直接手工改动底层表。</p>
 */
public final class ExecutableModule {
    private final List<Function> functions;
    private final NavigableMap<String, FunctionId> functionIds;
    private final List<Integer> functionGenericParams;

    private final List<HostImport> hostImports;
    private final NavigableMap<String, HostImportId> hostImportIds;

    private final List<String> typeNames;
    private final NavigableMap<String, TypeId> typeIds;

    private final List<String> methodNames;
    private final NavigableMap<String, MethodId> methodIds;

    // vcall_dispatch[type_id] = sorted unique list of (method_id, fn_id).
    private final List<List<VCallEntry>> vcallDispatch;
    // assoc_type_dispatch[type_id] = sorted unique list of (iface_type_id, assoc_name, fn_id).
    private final List<List<AssocTypeEntry>> assocTypeDispatch;
    // interface_impls[type_id] = sorted unique list of implemented interface TypeIds.
    private final List<List<TypeId>> interfaceImpls;
    // struct_layouts[type_id] = null for non-struct, or field names for struct types.
    private final List<List<String>> structLayouts;

    private final List<ExternalEffectDecl> externalEffects;
    private final NavigableMap<EffectKey, EffectId> externalEffectIds;

    private FunctionId entry;

    public ExecutableModule() {
        this(
                new ArrayList<>(),
                new TreeMap<>(),
                new ArrayList<>(),
                new ArrayList<>(),
                new TreeMap<>(),
                new ArrayList<>(),
                new TreeMap<>(),
                new ArrayList<>(),
                new TreeMap<>(),
                new ArrayList<>(),
                new ArrayList<>(),
                new ArrayList<>(),
                new ArrayList<>(),
                new ArrayList<>(),
                new TreeMap<>(),
                new FunctionId(0));

        for (String name : List.of("unit", "bool", "int", "float", "byte", "char", "string", "bytes")) {
            internType(name);
        }
    }

    ExecutableModule(
            List<Function> functions,
            NavigableMap<String, FunctionId> functionIds,
            List<Integer> functionGenericParams,
            List<HostImport> hostImports,
            NavigableMap<String, HostImportId> hostImportIds,
            List<String> typeNames,
            NavigableMap<String, TypeId> typeIds,
            List<String> methodNames,
            NavigableMap<String, MethodId> methodIds,
            List<List<VCallEntry>> vcallDispatch,
            List<List<AssocTypeEntry>> assocTypeDispatch,
            List<List<TypeId>> interfaceImpls,
            List<List<String>> structLayouts,
            List<ExternalEffectDecl> externalEffects,
            NavigableMap<EffectKey, EffectId> externalEffectIds,
            FunctionId entry) {
        this.functions = Objects.requireNonNull(functions, "functions");
        this.functionIds = Objects.requireNonNull(functionIds, "functionIds");
        this.functionGenericParams = Objects.requireNonNull(functionGenericParams, "functionGenericParams");
        this.hostImports = Objects.requireNonNull(hostImports, "hostImports");
        this.hostImportIds = Objects.requireNonNull(hostImportIds, "hostImportIds");
        this.typeNames = Objects.requireNonNull(typeNames, "typeNames");
        this.typeIds = Objects.requireNonNull(typeIds, "typeIds");
        this.methodNames = Objects.requireNonNull(methodNames, "methodNames");
        this.methodIds = Objects.requireNonNull(methodIds, "methodIds");
        this.vcallDispatch = Objects.requireNonNull(vcallDispatch, "vcallDispatch");
        this.assocTypeDispatch = Objects.requireNonNull(assocTypeDispatch, "assocTypeDispatch");
        this.interfaceImpls = Objects.requireNonNull(interfaceImpls, "interfaceImpls");
        this.structLayouts = Objects.requireNonNull(structLayouts, "structLayouts");
        this.externalEffects = Objects.requireNonNull(externalEffects, "externalEffects");
        this.externalEffectIds = Objects.requireNonNull(externalEffectIds, "externalEffectIds");
        this.entry = Objects.requireNonNull(entry, "entry");
    }

    public List<Function> functions() {
        return Collections.unmodifiableList(functions);
    }

    public NavigableMap<String, FunctionId> functionIds() {
        return Collections.unmodifiableNavigableMap(functionIds);
    }

    public List<Integer> functionGenericParams() {
        return Collections.unmodifiableList(functionGenericParams);
    }

    public List<HostImport> hostImports() {
        return Collections.unmodifiableList(hostImports);
    }

    public NavigableMap<String, HostImportId> hostImportIds() {
        return Collections.unmodifiableNavigableMap(hostImportIds);
    }

    public List<String> typeNames() {
        return Collections.unmodifiableList(typeNames);
    }

    public NavigableMap<String, TypeId> typeIds() {
        return Collections.unmodifiableNavigableMap(typeIds);
    }

    public List<String> methodNames() {
        return Collections.unmodifiableList(methodNames);
    }

    public NavigableMap<String, MethodId> methodIds() {
        return Collections.unmodifiableNavigableMap(methodIds);
    }

    public List<List<VCallEntry>> vcallDispatch() {
        return Collections.unmodifiableList(vcallDispatch);
    }

    public List<List<AssocTypeEntry>> assocTypeDispatch() {
        return Collections.unmodifiableList(assocTypeDispatch);
    }

    public List<List<TypeId>> interfaceImpls() {
        return Collections.unmodifiableList(interfaceImpls);
    }

    public List<List<String>> structLayouts() {
        return Collections.unmodifiableList(structLayouts);
    }

    public List<ExternalEffectDecl> externalEffects() {
        return Collections.unmodifiableList(externalEffects);
    }

    public NavigableMap<EffectKey, EffectId> externalEffectIds() {
        return Collections.unmodifiableNavigableMap(externalEffectIds);
    }

    public FunctionId entry() {
        return entry;
    }

    public void setEntry(FunctionId entry) {
        this.entry = Objects.requireNonNull(entry, "entry");
    }

    public Optional<Function> function(FunctionId id) {
        int idx = id.index();
        if (idx < 0 || idx >= functions.size()) {
            return Optional.empty();
        }
        return Optional.of(functions.get(idx));
    }

    public Optional<Integer> functionGenericParamCount(FunctionId id) {
        int idx = id.index();
        if (idx < 0 || idx >= functionGenericParams.size()) {
            return Optional.empty();
        }
        return Optional.of(functionGenericParams.get(idx));
    }

    public Optional<HostImport> hostImport(HostImportId id) {
        int idx = id.index();
        if (idx < 0 || idx >= hostImports.size()) {
            return Optional.empty();
        }
        return Optional.of(hostImports.get(idx));
    }

    public Optional<ExternalEffectDecl> externalEffect(EffectId id) {
        int idx = id.index();
        if (idx < 0 || idx >= externalEffects.size()) {
            return Optional.empty();
        }
        return Optional.of(externalEffects.get(idx));
    }

    public Optional<FunctionId> functionId(String name) {
        return Optional.ofNullable(functionIds.get(name));
    }

    public Optional<HostImportId> hostImportId(String name) {
        return Optional.ofNullable(hostImportIds.get(name));
    }

    public Optional<EffectId> externalEffectId(String interfaceName, String method) {
        return Optional.ofNullable(externalEffectIds.get(new EffectKey(interfaceName, method)));
    }

    public FunctionId addFunction(Function func) {
        Objects.requireNonNull(func, "func");
        if (functionIds.containsKey(func.name())) {
            throw new IllegalArgumentException("duplicate function `" + func.name() + "`");
        }
        int id = functions.size();
        FunctionId fnId = new FunctionId(id);
        functions.add(func);
        functionIds.put(func.name(), fnId);
        functionGenericParams.add(0);
        return fnId;
    }

    public void setFunctionGenericParamCount(FunctionId id, int count) {
        if (count < 0) {
            throw new IllegalArgumentException("generic param count must be >= 0");
        }
        int idx = id.index();
        if (idx < 0 || idx >= functionGenericParams.size()) {
            throw new IllegalArgumentException("function id out of range: " + idx);
        }
        functionGenericParams.set(idx, count);
    }

    public HostImportId addHostImport(HostImport import_) {
        Objects.requireNonNull(import_, "import");
        if (hostImportIds.containsKey(import_.name())) {
            throw new IllegalArgumentException("duplicate host import `" + import_.name() + "`");
        }
        int id = hostImports.size();
        HostImportId hid = new HostImportId(id);
        hostImports.add(import_);
        hostImportIds.put(import_.name(), hid);
        return hid;
    }

    public TypeId internType(String name) {
        Objects.requireNonNull(name, "name");
        if (name.isEmpty()) {
            throw new IllegalArgumentException("cannot intern empty type name");
        }
        TypeId existing = typeIds.get(name);
        if (existing != null) {
            return existing;
        }
        int id = typeNames.size();
        TypeId typeId = new TypeId(id);
        typeNames.add(name);
        typeIds.put(name, typeId);
        vcallDispatch.add(new ArrayList<>());
        assocTypeDispatch.add(new ArrayList<>());
        interfaceImpls.add(new ArrayList<>());
        structLayouts.add(null);
        return typeId;
    }

    public Optional<TypeId> typeId(String name) {
        Objects.requireNonNull(name, "name");
        return Optional.ofNullable(typeIds.get(name));
    }

    public Optional<String> typeName(TypeId id) {
        Objects.requireNonNull(id, "id");
        int idx = id.index();
        if (idx < 0 || idx >= typeNames.size()) {
            return Optional.empty();
        }
        return Optional.of(typeNames.get(idx));
    }

    public MethodId internMethod(String name) {
        Objects.requireNonNull(name, "name");
        if (name.isEmpty()) {
            throw new IllegalArgumentException("cannot intern empty method name");
        }
        MethodId existing = methodIds.get(name);
        if (existing != null) {
            return existing;
        }
        int id = methodNames.size();
        MethodId methodId = new MethodId(id);
        methodNames.add(name);
        methodIds.put(name, methodId);
        return methodId;
    }

    public Optional<MethodId> methodId(String name) {
        Objects.requireNonNull(name, "name");
        return Optional.ofNullable(methodIds.get(name));
    }

    public Optional<String> methodName(MethodId id) {
        Objects.requireNonNull(id, "id");
        int idx = id.index();
        if (idx < 0 || idx >= methodNames.size()) {
            return Optional.empty();
        }
        return Optional.of(methodNames.get(idx));
    }

    public void addVCallEntry(TypeId typeId, MethodId methodId, FunctionId fnId) {
        Objects.requireNonNull(typeId, "typeId");
        Objects.requireNonNull(methodId, "methodId");
        Objects.requireNonNull(fnId, "fnId");

        int idx = typeId.index();
        if (idx < 0 || idx >= vcallDispatch.size()) {
            throw new IllegalArgumentException("invalid TypeId " + idx);
        }
        List<VCallEntry> entries = vcallDispatch.get(idx);
        int pos = java.util.Collections.binarySearch(entries, new VCallEntry(methodId, fnId));
        if (pos >= 0) {
            throw new IllegalArgumentException(
                    "duplicate vcall dispatch entry for type " + typeId.index() + " method " + methodId.index());
        }
        entries.add(-pos - 1, new VCallEntry(methodId, fnId));
    }

    public Optional<FunctionId> vcallTarget(TypeId typeId, MethodId methodId) {
        Objects.requireNonNull(typeId, "typeId");
        Objects.requireNonNull(methodId, "methodId");
        int idx = typeId.index();
        if (idx < 0 || idx >= vcallDispatch.size()) {
            return Optional.empty();
        }
        List<VCallEntry> entries = vcallDispatch.get(idx);
        int pos = java.util.Collections.binarySearch(entries, new VCallEntry(methodId, new FunctionId(0)));
        if (pos < 0) {
            return Optional.empty();
        }
        return Optional.of(entries.get(pos).function());
    }

    public void addAssocTypeEntry(TypeId typeId, TypeId ifaceTypeId, String assoc, FunctionId fnId) {
        Objects.requireNonNull(typeId, "typeId");
        Objects.requireNonNull(ifaceTypeId, "ifaceTypeId");
        Objects.requireNonNull(assoc, "assoc");
        Objects.requireNonNull(fnId, "fnId");

        if (assoc.isEmpty()) {
            throw new IllegalArgumentException("cannot add assoc type entry with empty assoc name");
        }

        int idx = typeId.index();
        if (idx < 0 || idx >= assocTypeDispatch.size()) {
            throw new IllegalArgumentException("invalid TypeId " + idx);
        }

        List<AssocTypeEntry> entries = assocTypeDispatch.get(idx);
        int pos = java.util.Collections.binarySearch(entries, new AssocTypeEntry(ifaceTypeId, assoc, fnId));
        if (pos >= 0) {
            throw new IllegalArgumentException(
                    "duplicate assoc type dispatch entry for type "
                            + typeId.index()
                            + " interface "
                            + ifaceTypeId.index()
                            + " assoc `"
                            + assoc
                            + "`");
        }
        entries.add(-pos - 1, new AssocTypeEntry(ifaceTypeId, assoc, fnId));
    }

    public Optional<FunctionId> assocTypeTarget(TypeId typeId, TypeId ifaceTypeId, String assoc) {
        Objects.requireNonNull(typeId, "typeId");
        Objects.requireNonNull(ifaceTypeId, "ifaceTypeId");
        Objects.requireNonNull(assoc, "assoc");

        int idx = typeId.index();
        if (idx < 0 || idx >= assocTypeDispatch.size()) {
            return Optional.empty();
        }
        List<AssocTypeEntry> entries = assocTypeDispatch.get(idx);
        int pos =
                java.util.Collections.binarySearch(entries, new AssocTypeEntry(ifaceTypeId, assoc, new FunctionId(0)));
        if (pos < 0) {
            return Optional.empty();
        }
        return Optional.of(entries.get(pos).function());
    }

    public void setInterfaceImpls(TypeId typeId, List<TypeId> ifaces) {
        Objects.requireNonNull(typeId, "typeId");
        Objects.requireNonNull(ifaces, "ifaces");
        int idx = typeId.index();
        if (idx < 0 || idx >= interfaceImpls.size()) {
            throw new IllegalArgumentException("invalid TypeId " + idx);
        }
        ArrayList<TypeId> out = new ArrayList<>(ifaces);
        out.sort(java.util.Comparator.comparingInt(TypeId::index));
        out = new ArrayList<>(out.stream().distinct().toList());
        interfaceImpls.set(idx, out);
    }

    public Optional<List<TypeId>> interfaceImplsOf(TypeId typeId) {
        Objects.requireNonNull(typeId, "typeId");
        int idx = typeId.index();
        if (idx < 0 || idx >= interfaceImpls.size()) {
            return Optional.empty();
        }
        return Optional.of(Collections.unmodifiableList(interfaceImpls.get(idx)));
    }

    public void setStructLayout(TypeId typeId, List<String> fields) {
        Objects.requireNonNull(typeId, "typeId");
        Objects.requireNonNull(fields, "fields");
        int idx = typeId.index();
        if (idx < 0 || idx >= structLayouts.size()) {
            throw new IllegalArgumentException("invalid TypeId " + idx);
        }
        if (structLayouts.get(idx) != null) {
            throw new IllegalArgumentException("duplicate struct layout for type " + idx);
        }
        structLayouts.set(idx, List.copyOf(fields));
    }

    public Optional<List<String>> structLayout(TypeId typeId) {
        Objects.requireNonNull(typeId, "typeId");
        int idx = typeId.index();
        if (idx < 0 || idx >= structLayouts.size()) {
            return Optional.empty();
        }
        List<String> fields = structLayouts.get(idx);
        if (fields == null) {
            return Optional.empty();
        }
        return Optional.of(Collections.unmodifiableList(fields));
    }

    public EffectId addExternalEffect(ExternalEffectDecl decl) {
        Objects.requireNonNull(decl, "decl");
        EffectKey key = new EffectKey(decl.interfaceName(), decl.method());
        if (externalEffectIds.containsKey(key)) {
            throw new IllegalArgumentException(
                    "duplicate external effect `" + decl.interfaceName() + "." + decl.method() + "`");
        }
        int id = externalEffects.size();
        EffectId eid = new EffectId(id);
        externalEffects.add(decl);
        externalEffectIds.put(key, eid);
        return eid;
    }

    public record VCallEntry(MethodId method, FunctionId function) implements Comparable<VCallEntry> {
        public VCallEntry {
            Objects.requireNonNull(method, "method");
            Objects.requireNonNull(function, "function");
        }

        @Override
        public int compareTo(VCallEntry o) {
            return Integer.compare(method.index(), o.method.index());
        }
    }

    public record AssocTypeEntry(TypeId ifaceType, String assoc, FunctionId function) implements Comparable<AssocTypeEntry> {
        public AssocTypeEntry {
            Objects.requireNonNull(ifaceType, "ifaceType");
            Objects.requireNonNull(assoc, "assoc");
            Objects.requireNonNull(function, "function");
        }

        @Override
        public int compareTo(AssocTypeEntry o) {
            int c = Integer.compare(ifaceType.index(), o.ifaceType.index());
            if (c != 0) {
                return c;
            }
            return assoc.compareTo(o.assoc);
        }
    }

    public record EffectKey(String interfaceName, String method) implements Comparable<EffectKey> {
        public EffectKey {
            Objects.requireNonNull(interfaceName, "interfaceName");
            Objects.requireNonNull(method, "method");
        }

        @Override
        public int compareTo(EffectKey o) {
            int c = interfaceName.compareTo(o.interfaceName);
            if (c != 0) {
                return c;
            }
            return method.compareTo(o.method);
        }
    }
}
