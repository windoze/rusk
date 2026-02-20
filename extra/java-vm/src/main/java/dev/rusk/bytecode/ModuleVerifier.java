package dev.rusk.bytecode;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * `.rbc` 解码后的基础校验（对齐 Rust 侧 `rusk_bytecode::verify` 的主要不变量）。
 *
 * <p>注意：这是结构一致性/索引范围层面的 verifier，不做类型推断。</p>
 */
public final class ModuleVerifier {
    private ModuleVerifier() {}

    public static void verify(ExecutableModule module) throws VerifyException {
        // Table length consistency.
        if (module.functionGenericParams().size() != module.functions().size()) {
            throw new VerifyException(
                    "function_generic_params length "
                            + module.functionGenericParams().size()
                            + " does not match functions length "
                            + module.functions().size());
        }

        // Entry validity.
        if (module.entry().index() < 0 || module.entry().index() >= module.functions().size()) {
            throw new VerifyException(
                    "entry function id "
                            + module.entry().index()
                            + " out of range (functions="
                            + module.functions().size()
                            + ")");
        }

        // Recompute name -> id maps and ensure they match the stored ones.
        Map<String, FunctionId> expectedFunctionIds = new TreeMap<>();
        for (int i = 0; i < module.functions().size(); i++) {
            Function func = module.functions().get(i);
            FunctionId id = new FunctionId(i);
            if (expectedFunctionIds.put(func.name(), id) != null) {
                throw new VerifyException("duplicate function name `" + func.name() + "`");
            }
        }
        if (!expectedFunctionIds.equals(module.functionIds())) {
            throw new VerifyException("function_ids map does not match functions table");
        }

        Map<String, HostImportId> expectedHostImportIds = new TreeMap<>();
        for (int i = 0; i < module.hostImports().size(); i++) {
            HostImport imp = module.hostImports().get(i);
            HostImportId id = new HostImportId(i);
            if (expectedHostImportIds.put(imp.name(), id) != null) {
                throw new VerifyException("duplicate host import name `" + imp.name() + "`");
            }
        }
        if (!expectedHostImportIds.equals(module.hostImportIds())) {
            throw new VerifyException("host_import_ids map does not match host_imports table");
        }

        Map<ExecutableModule.EffectKey, EffectId> expectedExternalEffectIds = new TreeMap<>();
        for (int i = 0; i < module.externalEffects().size(); i++) {
            ExternalEffectDecl decl = module.externalEffects().get(i);
            EffectId id = new EffectId(i);
            ExecutableModule.EffectKey key =
                    new ExecutableModule.EffectKey(decl.interfaceName(), decl.method());
            if (expectedExternalEffectIds.put(key, id) != null) {
                throw new VerifyException(
                        "duplicate external effect declaration `" + decl.interfaceName() + "." + decl.method() + "`");
            }
        }
        if (!expectedExternalEffectIds.equals(module.externalEffectIds())) {
            throw new VerifyException("external_effect_ids map does not match external_effects table");
        }

        // Parallel tables must match the type table.
        int typesLen = module.typeNames().size();
        if (module.vcallDispatch().size() != typesLen) {
            throw new VerifyException(
                    "vcall_dispatch length "
                            + module.vcallDispatch().size()
                            + " does not match type_names length "
                            + typesLen);
        }
        if (module.interfaceImpls().size() != typesLen) {
            throw new VerifyException(
                    "interface_impls length "
                            + module.interfaceImpls().size()
                            + " does not match type_names length "
                            + typesLen);
        }
        if (module.structLayouts().size() != typesLen) {
            throw new VerifyException(
                    "struct_layouts length "
                            + module.structLayouts().size()
                            + " does not match type_names length "
                            + typesLen);
        }

        // Recompute interning maps and ensure they match the stored ones.
        Map<String, TypeId> expectedTypeIds = new TreeMap<>();
        for (int i = 0; i < module.typeNames().size(); i++) {
            String name = module.typeNames().get(i);
            if (name.isEmpty()) {
                throw new VerifyException("type_names contains empty type name");
            }
            TypeId id = new TypeId(i);
            if (expectedTypeIds.put(name, id) != null) {
                throw new VerifyException("duplicate type name `" + name + "`");
            }
        }
        if (!expectedTypeIds.equals(module.typeIds())) {
            throw new VerifyException("type_ids map does not match type_names table");
        }

        Map<String, MethodId> expectedMethodIds = new TreeMap<>();
        for (int i = 0; i < module.methodNames().size(); i++) {
            String name = module.methodNames().get(i);
            if (name.isEmpty()) {
                throw new VerifyException("method_names contains empty method name");
            }
            MethodId id = new MethodId(i);
            if (expectedMethodIds.put(name, id) != null) {
                throw new VerifyException("duplicate method name `" + name + "`");
            }
        }
        if (!expectedMethodIds.equals(module.methodIds())) {
            throw new VerifyException("method_ids map does not match method_names table");
        }

        // Ensure primitive type names are present.
        for (String name : List.of("unit", "bool", "int", "float", "byte", "char", "string", "bytes")) {
            if (module.typeId(name).isEmpty()) {
                throw new VerifyException("missing required primitive type name `" + name + "` in type table");
            }
        }

        // VCall dispatch table integrity: strictly sorted/unique by MethodId, and ids in range.
        int methodCount = module.methodNames().size();
        for (int typeIdx = 0; typeIdx < module.vcallDispatch().size(); typeIdx++) {
            List<ExecutableModule.VCallEntry> entries = module.vcallDispatch().get(typeIdx);
            Integer prev = null;
            for (ExecutableModule.VCallEntry e : entries) {
                int mid = e.method().index();
                int fid = e.function().index();
                if (mid < 0 || mid >= methodCount) {
                    throw new VerifyException(
                            "vcall dispatch entry for TypeId "
                                    + typeIdx
                                    + " has invalid MethodId "
                                    + mid
                                    + " (methods="
                                    + methodCount
                                    + ")");
                }
                if (prev != null && prev.intValue() >= mid) {
                    throw new VerifyException(
                            "vcall dispatch list for TypeId "
                                    + typeIdx
                                    + " is not strictly sorted/unique by MethodId");
                }
                if (fid < 0 || fid >= module.functions().size()) {
                    throw new VerifyException(
                            "vcall dispatch entry (TypeId "
                                    + typeIdx
                                    + ", MethodId "
                                    + mid
                                    + ") points to invalid function id "
                                    + fid);
                }
                prev = mid;
            }
        }

        // Interface impl tables are sorted unique lists of TypeId.
        int typeCount = module.typeNames().size();
        for (int typeIdx = 0; typeIdx < module.interfaceImpls().size(); typeIdx++) {
            List<TypeId> ifaces = module.interfaceImpls().get(typeIdx);
            Integer prev = null;
            for (TypeId ifaceId : ifaces) {
                int iid = ifaceId.index();
                if (iid < 0 || iid >= typeCount) {
                    throw new VerifyException(
                            "interface_impls for TypeId "
                                    + typeIdx
                                    + " contains invalid interface TypeId "
                                    + iid
                                    + " (types="
                                    + typeCount
                                    + ")");
                }
                if (prev != null && prev.intValue() >= iid) {
                    throw new VerifyException("interface_impls list for TypeId " + typeIdx + " is not strictly sorted/unique");
                }
                prev = iid;
            }
        }

        // Struct layout constraints.
        for (int typeIdx = 0; typeIdx < module.structLayouts().size(); typeIdx++) {
            List<String> fields = module.structLayouts().get(typeIdx);
            if (fields == null) {
                continue;
            }
            for (String field : fields) {
                if (field.isEmpty()) {
                    String typeName = module.typeName(new TypeId(typeIdx)).orElse("<unknown>");
                    throw new VerifyException("struct_layouts for `" + typeName + "` contains empty field name");
                }
            }
        }

        List<Integer> returnArities = computeReturnArities(module);

        // Function bodies.
        for (int fnIdx = 0; fnIdx < module.functions().size(); fnIdx++) {
            verifyFunction(module, new FunctionId(fnIdx), module.functions().get(fnIdx));
        }

        verifyCallReturnArities(module, returnArities);
    }

    private static List<Integer> computeReturnArities(ExecutableModule module) throws VerifyException {
        enum ReturnKind {
            SINGLE,
            MULTI
        }

        List<Integer> out = new ArrayList<>(module.functions().size());
        for (Function func : module.functions()) {
            ReturnKind kind = null;
            Integer multiArity = null;
            for (Instruction inst : func.code()) {
                if (inst instanceof Instruction.Return) {
                    if (kind == ReturnKind.MULTI) {
                        throw new VerifyException("function `" + func.name() + "` mixes Return and ReturnMulti");
                    }
                    kind = ReturnKind.SINGLE;
                } else if (inst instanceof Instruction.ReturnMulti rm) {
                    int arity = rm.values().size();
                    if (arity == 0) {
                        throw new VerifyException(
                                "function `" + func.name() + "` has invalid ReturnMulti with 0 values");
                    }
                    if (kind == ReturnKind.SINGLE) {
                        throw new VerifyException("function `" + func.name() + "` mixes Return and ReturnMulti");
                    }
                    kind = ReturnKind.MULTI;
                    if (multiArity == null) {
                        multiArity = arity;
                    } else if (multiArity != arity) {
                        throw new VerifyException(
                                "function `"
                                        + func.name()
                                        + "` has inconsistent ReturnMulti arity: expected "
                                        + multiArity
                                        + ", got "
                                        + arity);
                    }
                }
            }

            int arity = (kind == null || kind == ReturnKind.SINGLE) ? 1 : multiArity;
            out.add(arity);
        }
        return out;
    }

    private static void verifyCallReturnArities(ExecutableModule module, List<Integer> returnArities)
            throws VerifyException {
        for (Function func : module.functions()) {
            for (int pc = 0; pc < func.code().size(); pc++) {
                Instruction inst = func.code().get(pc);
                String here = "function `" + func.name() + "` pc " + pc;
                if (inst instanceof Instruction.Call call) {
                    if (!(call.func() instanceof CallTarget.Bc bc)) {
                        continue;
                    }
                    int callee = bc.function().index();
                    int calleeArity = (callee >= 0 && callee < returnArities.size()) ? returnArities.get(callee) : 1;
                    if (calleeArity != 1) {
                        String calleeName =
                                module.function(bc.function()).map(Function::name).orElse("<unknown>");
                        throw new VerifyException(
                                here
                                        + ": Call expects single return, but callee `"
                                        + calleeName
                                        + "` returns "
                                        + calleeArity
                                        + " values");
                    }
                } else if (inst instanceof Instruction.CallMulti cm) {
                    if (!(cm.func() instanceof CallTarget.Bc bc)) {
                        throw new VerifyException(here + ": CallMulti only supports bytecode functions");
                    }
                    int expected = cm.dsts().size();
                    if (expected == 0) {
                        throw new VerifyException(here + ": CallMulti requires at least one dst register");
                    }
                    int callee = bc.function().index();
                    int calleeArity =
                            (callee >= 0 && callee < returnArities.size()) ? returnArities.get(callee) : 1;
                    if (calleeArity != expected) {
                        String calleeName =
                                module.function(bc.function()).map(Function::name).orElse("<unknown>");
                        throw new VerifyException(
                                here
                                        + ": CallMulti dst count "
                                        + expected
                                        + " does not match callee `"
                                        + calleeName
                                        + "` return arity "
                                        + calleeArity);
                    }
                }
            }
        }
    }

    private static void verifyFunction(ExecutableModule module, FunctionId fnId, Function func) throws VerifyException {
        if (func.paramCount() > func.regCount()) {
            throw new VerifyException(
                    "function `"
                            + func.name()
                            + "` has param_count "
                            + func.paramCount()
                            + " > reg_count "
                            + func.regCount());
        }

        int codeLen = func.code().size();

        int regCount = func.regCount();
        for (int pc = 0; pc < codeLen; pc++) {
            verifyInstruction(module, func, fnId, pc, codeLen, regCount, func.code().get(pc));
        }
    }

    private static void verifyReg(int regCount, int reg, String context) throws VerifyException {
        if (reg < 0 || reg >= regCount) {
            throw new VerifyException(context + ": reg " + reg + " out of range (reg_count=" + regCount + ")");
        }
    }

    private static void verifyPc(int codeLen, int pc, String context) throws VerifyException {
        if (pc < 0 || pc >= codeLen) {
            throw new VerifyException(context + ": pc " + pc + " out of range (code_len=" + codeLen + ")");
        }
    }

    private static int countPatternBinds(Pattern p) {
        if (p instanceof Pattern.Wildcard) {
            return 0;
        }
        if (p instanceof Pattern.Bind) {
            return 1;
        }
        if (p instanceof Pattern.Literal) {
            return 0;
        }
        if (p instanceof Pattern.Tuple t) {
            int n = 0;
            for (Pattern pp : t.prefix()) {
                n += countPatternBinds(pp);
            }
            if (t.rest() != null) {
                n += countPatternBinds(t.rest());
            }
            for (Pattern pp : t.suffix()) {
                n += countPatternBinds(pp);
            }
            return n;
        }
        if (p instanceof Pattern.Enum e) {
            int n = 0;
            for (Pattern pp : e.fields()) {
                n += countPatternBinds(pp);
            }
            return n;
        }
        if (p instanceof Pattern.Struct s) {
            int n = 0;
            for (Pattern.Struct.Field f : s.fields()) {
                n += countPatternBinds(f.pattern());
            }
            return n;
        }
        if (p instanceof Pattern.Array a) {
            int n = 0;
            for (Pattern pp : a.prefix()) {
                n += countPatternBinds(pp);
            }
            if (a.rest() != null) {
                n += countPatternBinds(a.rest());
            }
            for (Pattern pp : a.suffix()) {
                n += countPatternBinds(pp);
            }
            return n;
        }
        throw new IllegalStateException("unknown Pattern: " + p);
    }

    private static void verifyInstruction(
            ExecutableModule module,
            Function func,
            FunctionId fnId,
            int pc,
            int codeLen,
            int regCount,
            Instruction inst)
            throws VerifyException {
        String here = "function `" + func.name() + "` pc " + pc;

        if (inst instanceof Instruction.Const c) {
            verifyReg(regCount, c.dst(), here + ": const dst");
            if (c.value() instanceof ConstValue.TypeRep t) {
                verifyTypeRepLitInterned(module, t.value(), here + ": const typerep");
            }
        } else if (inst instanceof Instruction.Copy i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.src(), here + ": src");
        } else if (inst instanceof Instruction.Move i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.src(), here + ": src");
        } else if (inst instanceof Instruction.AsReadonly i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.src(), here + ": src");
        } else if (inst instanceof Instruction.IsType i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.value(), here + ": value");
            verifyReg(regCount, i.ty(), here + ": ty");
        } else if (inst instanceof Instruction.MakeTypeRep i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyTypeRepLitInterned(module, i.base(), here + ": MakeTypeRep base");
            for (int r : i.args()) {
                verifyReg(regCount, r, here + ": MakeTypeRep arg");
            }
        } else if (inst instanceof Instruction.MakeStruct i) {
            verifyTypeId(module, i.typeId(), here + ": type id");
            List<String> layout = module.structLayouts().get(i.typeId().index());
            if (layout == null) {
                String typeName = module.typeName(i.typeId()).orElse("<unknown>");
                throw new VerifyException(here + ": missing struct layout for `" + typeName + "`");
            }
            verifyReg(regCount, i.dst(), here + ": dst");
            for (int r : i.typeArgs()) {
                verifyReg(regCount, r, here + ": type arg");
            }
            for (Instruction.MakeStruct.FieldInit f : i.fields()) {
                verifyReg(regCount, f.reg(), here + ": field value");
            }
        } else if (inst instanceof Instruction.MakeArray i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            for (int r : i.items()) {
                verifyReg(regCount, r, here + ": item");
            }
        } else if (inst instanceof Instruction.MakeTuple i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            for (int r : i.items()) {
                verifyReg(regCount, r, here + ": item");
            }
        } else if (inst instanceof Instruction.MakeEnum i) {
            verifyTypeId(module, i.enumTypeId(), here + ": enum type id");
            verifyReg(regCount, i.dst(), here + ": dst");
            for (int r : i.typeArgs()) {
                verifyReg(regCount, r, here + ": type arg");
            }
            for (int r : i.fields()) {
                verifyReg(regCount, r, here + ": enum field");
            }
        } else if (inst instanceof Instruction.GetField i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.obj(), here + ": obj");
        } else if (inst instanceof Instruction.SetField i) {
            verifyReg(regCount, i.obj(), here + ": obj");
            verifyReg(regCount, i.value(), here + ": value");
        } else if (inst instanceof Instruction.StructGet i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.obj(), here + ": obj");
        } else if (inst instanceof Instruction.StructSet i) {
            verifyReg(regCount, i.obj(), here + ": obj");
            verifyReg(regCount, i.value(), here + ": value");
        } else if (inst instanceof Instruction.TupleGet i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.tup(), here + ": tup");
        } else if (inst instanceof Instruction.TupleSet i) {
            verifyReg(regCount, i.tup(), here + ": tup");
            verifyReg(regCount, i.value(), here + ": value");
        } else if (inst instanceof Instruction.IndexGet i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.arr(), here + ": arr");
            verifyReg(regCount, i.idx(), here + ": idx");
        } else if (inst instanceof Instruction.IndexSet i) {
            verifyReg(regCount, i.arr(), here + ": arr");
            verifyReg(regCount, i.idx(), here + ": idx");
            verifyReg(regCount, i.value(), here + ": value");
        } else if (inst instanceof Instruction.Len i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.arr(), here + ": arr");
        } else if (inst instanceof Instruction.IntAdd i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntSub i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntMul i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntDiv i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntMod i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntLt i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntLe i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntGt i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntGe i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntEq i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntNe i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntAnd i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntOr i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntXor i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntShl i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntShr i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntUShr i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.IntNot i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.v(), here + ": v");
        } else if (inst instanceof Instruction.ByteAnd i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.ByteOr i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.ByteXor i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.ByteShl i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.ByteShr i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.ByteUShr i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.ByteNot i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.v(), here + ": v");
        } else if (inst instanceof Instruction.BoolNot i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.v(), here + ": v");
        } else if (inst instanceof Instruction.BoolEq i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.BoolNe i) {
            verifyReg(regCount, i.dst(), here + ": dst");
            verifyReg(regCount, i.a(), here + ": a");
            verifyReg(regCount, i.b(), here + ": b");
        } else if (inst instanceof Instruction.Call i) {
            if (i.dst() != null) {
                verifyReg(regCount, i.dst(), here + ": dst");
            }
            verifyCallTarget(module, i.func(), here);
            for (int r : i.args()) {
                verifyReg(regCount, r, here + ": arg");
            }
        } else if (inst instanceof Instruction.CallMulti i) {
            for (int dst : i.dsts()) {
                verifyReg(regCount, dst, here + ": dst");
            }
            verifyCallTarget(module, i.func(), here);
            for (int r : i.args()) {
                verifyReg(regCount, r, here + ": arg");
            }
        } else if (inst instanceof Instruction.ICall i) {
            if (i.dst() != null) {
                verifyReg(regCount, i.dst(), here + ": dst");
            }
            verifyReg(regCount, i.fnptr(), here + ": fnptr");
            for (int r : i.args()) {
                verifyReg(regCount, r, here + ": arg");
            }
        } else if (inst instanceof Instruction.VCall i) {
            verifyMethodId(module, i.method(), here + ": method id");
            if (i.dst() != null) {
                verifyReg(regCount, i.dst(), here + ": dst");
            }
            verifyReg(regCount, i.obj(), here + ": obj");
            for (int r : i.methodTypeArgs()) {
                verifyReg(regCount, r, here + ": method type arg");
            }
            for (int r : i.args()) {
                verifyReg(regCount, r, here + ": arg");
            }
        } else if (inst instanceof Instruction.PushHandler i) {
            for (HandlerClause clause : i.clauses()) {
                verifyEffectSpec(regCount, clause.effect(), here + ": handler effect");
                verifyPc(codeLen, clause.targetPc(), here + ": handler target");
                for (int r : clause.paramRegs()) {
                    verifyReg(regCount, r, here + ": handler param reg");
                }
                int bindCount = 0;
                for (Pattern p : clause.argPatterns()) {
                    bindCount += countPatternBinds(p);
                }
                int expectedMin = bindCount;
                int expectedMax = bindCount + 1;
                if (clause.paramRegs().size() != expectedMin && clause.paramRegs().size() != expectedMax) {
                    throw new VerifyException(
                            here
                                    + ": handler clause param_regs length "
                                    + clause.paramRegs().size()
                                    + " does not match expected "
                                    + expectedMin
                                    + " or "
                                    + expectedMax);
                }
            }
        } else if (inst instanceof Instruction.PopHandler) {
            // ok
        } else if (inst instanceof Instruction.Perform i) {
            if (i.dst() != null) {
                verifyReg(regCount, i.dst(), here + ": dst");
            }
            verifyEffectSpec(regCount, i.effect(), here + ": perform effect");
            for (int r : i.args()) {
                verifyReg(regCount, r, here + ": perform arg");
            }
        } else if (inst instanceof Instruction.Resume i) {
            if (i.dst() != null) {
                verifyReg(regCount, i.dst(), here + ": dst");
            }
            verifyReg(regCount, i.k(), here + ": k");
            verifyReg(regCount, i.value(), here + ": value");
        } else if (inst instanceof Instruction.ResumeTail i) {
            verifyReg(regCount, i.k(), here + ": k");
            verifyReg(regCount, i.value(), here + ": value");
        } else if (inst instanceof Instruction.Jump i) {
            verifyPc(codeLen, i.targetPc(), here + ": jump");
        } else if (inst instanceof Instruction.JumpIf i) {
            verifyReg(regCount, i.cond(), here + ": cond");
            verifyPc(codeLen, i.thenPc(), here + ": then");
            verifyPc(codeLen, i.elsePc(), here + ": else");
        } else if (inst instanceof Instruction.Switch i) {
            verifyReg(regCount, i.value(), here + ": value");
            verifyPc(codeLen, i.defaultPc(), here + ": default");
            for (SwitchCase c : i.cases()) {
                verifyPc(codeLen, c.targetPc(), here + ": case target");
                for (int r : c.paramRegs()) {
                    verifyReg(regCount, r, here + ": case param");
                }
                int expected = countPatternBinds(c.pattern());
                if (c.paramRegs().size() != expected) {
                    throw new VerifyException(
                            here
                                    + ": switch case param_regs length "
                                    + c.paramRegs().size()
                                    + " does not match expected "
                                    + expected);
                }
            }
        } else if (inst instanceof Instruction.Return i) {
            verifyReg(regCount, i.value(), here + ": return value");
        } else if (inst instanceof Instruction.ReturnMulti i) {
            if (i.values().isEmpty()) {
                throw new VerifyException(here + ": ReturnMulti must return at least one value");
            }
            for (int r : i.values()) {
                verifyReg(regCount, r, here + ": return value");
            }
        } else if (inst instanceof Instruction.Trap) {
            // ok
        } else {
            throw new VerifyException(here + ": unknown instruction " + inst.getClass().getName());
        }

        // Verify that generic arity doesn't exceed param_count.
        int generic = module.functionGenericParams().get(fnId.index());
        if (func.paramCount() < generic) {
            throw new VerifyException(
                    "function `"
                            + func.name()
                            + "` has param_count "
                            + func.paramCount()
                            + " smaller than generic param count "
                            + generic);
        }
    }

    private static void verifyCallTarget(ExecutableModule module, CallTarget target, String context)
            throws VerifyException {
        if (target instanceof CallTarget.Bc bc) {
            int idx = bc.function().index();
            if (idx < 0 || idx >= module.functions().size()) {
                throw new VerifyException(context + ": invalid function id " + idx);
            }
        } else if (target instanceof CallTarget.Host host) {
            int idx = host.hostImport().index();
            if (idx < 0 || idx >= module.hostImports().size()) {
                throw new VerifyException(context + ": invalid host import id " + idx);
            }
        } else if (target instanceof CallTarget.IntrinsicTarget) {
            // ok
        } else {
            throw new VerifyException(context + ": invalid call target");
        }
    }

    private static void verifyEffectSpec(int regCount, EffectSpec spec, String context) throws VerifyException {
        if (spec.interfaceName().isEmpty()) {
            throw new VerifyException(context + ": empty interface name");
        }
        if (spec.method().isEmpty()) {
            throw new VerifyException(context + ": empty method name");
        }
        for (int r : spec.interfaceArgs()) {
            verifyReg(regCount, r, context + ": interface arg");
        }
    }

    private static void verifyTypeId(ExecutableModule module, TypeId id, String context) throws VerifyException {
        int idx = id.index();
        int len = module.typeNames().size();
        if (idx < 0 || idx >= len) {
            throw new VerifyException(context + ": TypeId " + idx + " out of range (types=" + len + ")");
        }
    }

    private static void verifyMethodId(ExecutableModule module, MethodId id, String context) throws VerifyException {
        int idx = id.index();
        int len = module.methodNames().size();
        if (idx < 0 || idx >= len) {
            throw new VerifyException(context + ": MethodId " + idx + " out of range (methods=" + len + ")");
        }
    }

    private static void verifyTypeRepLitInterned(ExecutableModule module, TypeRepLit lit, String context)
            throws VerifyException {
        if (lit instanceof TypeRepLit.Struct s) {
            if (module.typeId(s.name()).isEmpty()) {
                throw new VerifyException(context + ": unknown type name `" + s.name() + "` (not interned)");
            }
            return;
        }
        if (lit instanceof TypeRepLit.Enum e) {
            if (module.typeId(e.name()).isEmpty()) {
                throw new VerifyException(context + ": unknown type name `" + e.name() + "` (not interned)");
            }
            return;
        }
        if (lit instanceof TypeRepLit.Interface i) {
            if (module.typeId(i.name()).isEmpty()) {
                throw new VerifyException(context + ": unknown type name `" + i.name() + "` (not interned)");
            }
            return;
        }
    }
}
