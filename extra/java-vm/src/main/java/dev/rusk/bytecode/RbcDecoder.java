package dev.rusk.bytecode;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.TreeMap;
import java.util.TreeSet;

final class RbcDecoder {
    private static final byte[] MAGIC = new byte[] {'R', 'U', 'S', 'K', 'B', 'C', '0', 0};
    private static final int VERSION_MAJOR = 0;
    private static final int VERSION_MINOR = 12;

    private byte[] bytes;
    private int pos;

    ExecutableModule decode(byte[] bytes) throws RbcDecodeException {
        this.bytes = bytes;
        this.pos = 0;

        expectBytes(MAGIC, "bad magic");
        int major = readU16();
        int minor = readU16();
        if (major != VERSION_MAJOR || minor != VERSION_MINOR) {
            throw err(
                    "unsupported rbc version "
                            + major
                            + "."
                            + minor
                            + " (expected "
                            + VERSION_MAJOR
                            + "."
                            + VERSION_MINOR
                            + ")");
        }

        ExecutableModule module = readModule();
        try {
            ModuleVerifier.verify(module);
        } catch (VerifyException e) {
            throw err(e.getMessage());
        }

        if (remaining() != 0) {
            throw err("trailing bytes");
        }
        return module;
    }

    private RbcDecodeException err(String message) {
        return new RbcDecodeException(message, pos);
    }

    private int remaining() {
        return Math.max(0, bytes.length - pos);
    }

    private void expectBytes(byte[] expected, String message) throws RbcDecodeException {
        byte[] got = readExact(expected.length);
        for (int i = 0; i < expected.length; i++) {
            if (got[i] != expected[i]) {
                throw err(message);
            }
        }
    }

    private byte[] readExact(int n) throws RbcDecodeException {
        if (n < 0) {
            throw err("offset overflow");
        }
        int end = pos + n;
        if (end < pos || end > bytes.length) {
            throw err("unexpected EOF");
        }
        byte[] slice = new byte[n];
        System.arraycopy(bytes, pos, slice, 0, n);
        pos = end;
        return slice;
    }

    private int readU8() throws RbcDecodeException {
        return readExact(1)[0] & 0xFF;
    }

    private int readU16() throws RbcDecodeException {
        byte[] b = readExact(2);
        return ((b[1] & 0xFF) << 8) | (b[0] & 0xFF);
    }

    private long readU32() throws RbcDecodeException {
        byte[] b = readExact(4);
        return ((long) (b[3] & 0xFF) << 24)
                | ((long) (b[2] & 0xFF) << 16)
                | ((long) (b[1] & 0xFF) << 8)
                | ((long) (b[0] & 0xFF));
    }

    private long readU64() throws RbcDecodeException {
        byte[] b = readExact(8);
        long v = 0;
        for (int i = 0; i < 8; i++) {
            v |= (long) (b[i] & 0xFF) << (8 * i);
        }
        return v;
    }

    private long readI64() throws RbcDecodeException {
        return readU64();
    }

    private double readF64() throws RbcDecodeException {
        return Double.longBitsToDouble(readU64());
    }

    private boolean readBool() throws RbcDecodeException {
        int tag = readU8();
        return switch (tag) {
            case 0 -> false;
            case 1 -> true;
            default -> throw err("invalid bool tag " + tag);
        };
    }

    private int readLen() throws RbcDecodeException {
        long n = readU32();
        if (n > Integer.MAX_VALUE) {
            throw err("length overflow");
        }
        return (int) n;
    }

    private String readString() throws RbcDecodeException {
        int n = readLen();
        byte[] b = readExact(n);
        // Java 的 UTF-8 解码在遇到非法序列时会替换为 U+FFFD；这里为了对齐 Rust，主动检查。
        // 简化：先按 UTF-8 解码，再回编码比对；若不一致视为非法。
        String s = new String(b, StandardCharsets.UTF_8);
        byte[] roundtrip = s.getBytes(StandardCharsets.UTF_8);
        if (!java.util.Arrays.equals(b, roundtrip)) {
            throw err("invalid utf-8");
        }
        return s;
    }

    private byte[] readBlob() throws RbcDecodeException {
        int n = readLen();
        return readExact(n);
    }

    private List<Integer> readVecReg() throws RbcDecodeException {
        int n = readLen();
        // Each reg is u32.
        long minBytes = (long) n * 4L;
        if (minBytes > remaining()) {
            throw err("unexpected EOF");
        }
        List<Integer> out = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            long v = readU32();
            if (v > Integer.MAX_VALUE) {
                throw err("reg overflow");
            }
            out.add((int) v);
        }
        return out;
    }

    private Integer readOptionReg() throws RbcDecodeException {
        int tag = readU8();
        return switch (tag) {
            case 0 -> null;
            case 1 -> {
                long v = readU32();
                if (v > Integer.MAX_VALUE) {
                    throw err("reg overflow");
                }
                yield (int) v;
            }
            default -> throw err("invalid option tag " + tag);
        };
    }

    private ExecutableModule readModule() throws RbcDecodeException {
        // Functions
        int fnLen = readLen();
        List<Function> functions = new ArrayList<>(fnLen);
        for (int i = 0; i < fnLen; i++) {
            functions.add(readFunction());
        }

        int genLen = readLen();
        List<Integer> functionGenericParams = new ArrayList<>(genLen);
        for (int i = 0; i < genLen; i++) {
            long v = readU32();
            if (v > Integer.MAX_VALUE) {
                throw err("generic param overflow");
            }
            functionGenericParams.add((int) v);
        }

        // Host imports
        int hostLen = readLen();
        List<HostImport> hostImports = new ArrayList<>(hostLen);
        for (int i = 0; i < hostLen; i++) {
            hostImports.add(readHostImport());
        }

        // Interned type names.
        int typeLen = readLen();
        List<String> typeNames = new ArrayList<>(typeLen);
        for (int i = 0; i < typeLen; i++) {
            typeNames.add(readString());
        }
        NavigableMap<String, TypeId> typeIds = new TreeMap<>();
        for (int idx = 0; idx < typeNames.size(); idx++) {
            String name = typeNames.get(idx);
            TypeId id = new TypeId(idx);
            if (typeIds.put(name, id) != null) {
                throw err("duplicate type name `" + name + "`");
            }
        }

        // Interned method names.
        int methodLen = readLen();
        List<String> methodNames = new ArrayList<>(methodLen);
        for (int i = 0; i < methodLen; i++) {
            methodNames.add(readString());
        }
        NavigableMap<String, MethodId> methodIds = new TreeMap<>();
        for (int idx = 0; idx < methodNames.size(); idx++) {
            String name = methodNames.get(idx);
            MethodId id = new MethodId(idx);
            if (methodIds.put(name, id) != null) {
                throw err("duplicate method name `" + name + "`");
            }
        }

        // Dispatch table.
        ArrayList<List<ExecutableModule.VCallEntry>> vcallDispatch = new ArrayList<>(typeLen);
        for (int i = 0; i < typeLen; i++) {
            vcallDispatch.add(new ArrayList<>());
        }
        int dispatchLen = readLen();
        for (int i = 0; i < dispatchLen; i++) {
            int typeId = readU32Usize("type id overflow");
            int methodId = readU32Usize("method id overflow");
            int fnId = readU32Usize("function id overflow");
            if (typeId < 0 || typeId >= vcallDispatch.size()) {
                throw err("invalid TypeId " + typeId + " in dispatch entry");
            }
            vcallDispatch.get(typeId).add(new ExecutableModule.VCallEntry(new MethodId(methodId), new FunctionId(fnId)));
        }

        // Assoc type dispatch table.
        ArrayList<List<ExecutableModule.AssocTypeEntry>> assocTypeDispatch = new ArrayList<>(typeLen);
        for (int i = 0; i < typeLen; i++) {
            assocTypeDispatch.add(new ArrayList<>());
        }
        int assocLen = readLen();
        for (int i = 0; i < assocLen; i++) {
            int typeId = readU32Usize("type id overflow");
            int ifaceTypeId = readU32Usize("type id overflow");
            String assoc = readString();
            int fnId = readU32Usize("function id overflow");
            if (typeId < 0 || typeId >= assocTypeDispatch.size()) {
                throw err("invalid TypeId " + typeId + " in assoc type dispatch entry");
            }
            assocTypeDispatch
                    .get(typeId)
                    .add(new ExecutableModule.AssocTypeEntry(new TypeId(ifaceTypeId), assoc, new FunctionId(fnId)));
        }

        // Interface impls.
        ArrayList<List<TypeId>> interfaceImpls = new ArrayList<>(typeLen);
        for (int i = 0; i < typeLen; i++) {
            interfaceImpls.add(new ArrayList<>());
        }
        int implLen = readLen();
        for (int i = 0; i < implLen; i++) {
            int typeId = readU32Usize("type id overflow");
            if (typeId < 0 || typeId >= interfaceImpls.size()) {
                throw err("invalid TypeId " + typeId + " in interface impls");
            }
            int ifaceLen = readLen();
            ArrayList<TypeId> ifaces = new ArrayList<>(ifaceLen);
            for (int j = 0; j < ifaceLen; j++) {
                int ifaceId = readU32Usize("interface type id overflow");
                ifaces.add(new TypeId(ifaceId));
            }
            interfaceImpls.set(typeId, ifaces);
        }

        // Struct layouts.
        ArrayList<List<String>> structLayouts = new ArrayList<>(typeLen);
        for (int i = 0; i < typeLen; i++) {
            structLayouts.add(null);
        }
        int layoutLen = readLen();
        for (int i = 0; i < layoutLen; i++) {
            int typeId = readU32Usize("type id overflow");
            if (typeId < 0 || typeId >= structLayouts.size()) {
                throw err("invalid TypeId " + typeId + " in struct layouts");
            }
            if (structLayouts.get(typeId) != null) {
                throw err("duplicate struct layout entry for TypeId " + typeId);
            }
            int fieldLen = readLen();
            ArrayList<String> fields = new ArrayList<>(fieldLen);
            for (int j = 0; j < fieldLen; j++) {
                fields.add(readString());
            }
            structLayouts.set(typeId, List.copyOf(fields));
        }

        // External effects
        int effLen = readLen();
        List<ExternalEffectDecl> externalEffects = new ArrayList<>(effLen);
        for (int i = 0; i < effLen; i++) {
            externalEffects.add(readExternalEffectDecl());
        }

        long entry = readU32();
        if (entry > Integer.MAX_VALUE) {
            throw err("entry overflow");
        }
        FunctionId entryId = new FunctionId((int) entry);

        // Rebuild auxiliary maps.
        NavigableMap<String, FunctionId> functionIds = new TreeMap<>();
        for (int idx = 0; idx < functions.size(); idx++) {
            Function f = functions.get(idx);
            FunctionId id = new FunctionId(idx);
            if (functionIds.put(f.name(), id) != null) {
                throw err("duplicate function name `" + f.name() + "`");
            }
        }

        NavigableMap<String, HostImportId> hostImportIds = new TreeMap<>();
        for (int idx = 0; idx < hostImports.size(); idx++) {
            HostImport imp = hostImports.get(idx);
            HostImportId id = new HostImportId(idx);
            if (hostImportIds.put(imp.name(), id) != null) {
                throw err("duplicate host import name `" + imp.name() + "`");
            }
        }

        NavigableMap<ExecutableModule.EffectKey, EffectId> externalEffectIds = new TreeMap<>();
        for (int idx = 0; idx < externalEffects.size(); idx++) {
            ExternalEffectDecl decl = externalEffects.get(idx);
            EffectId id = new EffectId(idx);
            ExecutableModule.EffectKey key = new ExecutableModule.EffectKey(decl.interfaceName(), decl.method());
            if (externalEffectIds.put(key, id) != null) {
                throw err("duplicate external effect `" + decl.interfaceName() + "." + decl.method() + "`");
            }
        }

        return new ExecutableModule(
                functions,
                functionIds,
                functionGenericParams,
                hostImports,
                hostImportIds,
                typeNames,
                typeIds,
                methodNames,
                methodIds,
                vcallDispatch,
                assocTypeDispatch,
                interfaceImpls,
                structLayouts,
                externalEffects,
                externalEffectIds,
                entryId);
    }

    private Function readFunction() throws RbcDecodeException {
        String name = readString();
        long regCount = readU32();
        long paramCount = readU32();
        if (regCount > Integer.MAX_VALUE || paramCount > Integer.MAX_VALUE) {
            throw err("reg/param overflow");
        }
        int codeLen = readLen();
        List<Instruction> code = new ArrayList<>(codeLen);
        for (int i = 0; i < codeLen; i++) {
            code.add(readInstruction());
        }
        return new Function(name, (int) regCount, (int) paramCount, code);
    }

    private AbiType readAbiType() throws RbcDecodeException {
        int tag = readU8();
        try {
            return AbiType.fromTag(tag);
        } catch (IllegalArgumentException e) {
            throw err("invalid AbiType tag " + tag);
        }
    }

    private HostFnSig readHostFnSig() throws RbcDecodeException {
        int n = readLen();
        List<AbiType> params = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            params.add(readAbiType());
        }
        AbiType ret = readAbiType();
        return new HostFnSig(params, ret);
    }

    private HostImport readHostImport() throws RbcDecodeException {
        String name = readString();
        HostFnSig sig = readHostFnSig();
        return new HostImport(name, sig);
    }

    private ExternalEffectDecl readExternalEffectDecl() throws RbcDecodeException {
        String iface = readString();
        String method = readString();
        HostFnSig sig = readHostFnSig();
        return new ExternalEffectDecl(iface, method, sig);
    }

    private ConstValue readConstValue() throws RbcDecodeException {
        int tag = readU8();
        return switch (tag) {
            case 0 -> new ConstValue.Unit();
            case 1 -> new ConstValue.Bool(readBool());
            case 2 -> new ConstValue.Int(readI64());
            case 3 -> new ConstValue.Float(readF64());
            case 4 -> new ConstValue.Str(readString());
            case 5 -> new ConstValue.Bytes(readBlob());
            case 6 -> new ConstValue.TypeRep(readTypeRepLit());
            case 7 -> {
                long idx = readU32();
                if (idx > Integer.MAX_VALUE) {
                    throw err("function id overflow");
                }
                yield new ConstValue.Function(new FunctionId((int) idx));
            }
            default -> throw err("invalid ConstValue tag " + tag);
        };
    }

    private TypeRepLit readTypeRepLit() throws RbcDecodeException {
        int tag = readU8();
        return switch (tag) {
            case 0 -> new TypeRepLit.Unit();
            case 1 -> new TypeRepLit.Never();
            case 2 -> new TypeRepLit.Bool();
            case 3 -> new TypeRepLit.Int();
            case 4 -> new TypeRepLit.Float();
            case 5 -> new TypeRepLit.Byte();
            case 6 -> new TypeRepLit.Char();
            case 7 -> new TypeRepLit.String();
            case 8 -> new TypeRepLit.Bytes();
            case 9 -> new TypeRepLit.Array();
            case 10 -> {
                long arity = readU32();
                if (arity > Integer.MAX_VALUE) {
                    throw err("tuple arity overflow");
                }
                yield new TypeRepLit.Tuple((int) arity);
            }
            case 11 -> new TypeRepLit.Struct(readString());
            case 12 -> new TypeRepLit.Enum(readString());
            case 13 -> new TypeRepLit.Interface(readString());
            case 14 -> new TypeRepLit.Fn();
            case 15 -> new TypeRepLit.Cont();
            default -> throw err("invalid TypeRepLit tag " + tag);
        };
    }

    private Pattern readPattern() throws RbcDecodeException {
        int tag = readU8();
        return switch (tag) {
            case 0 -> new Pattern.Wildcard();
            case 1 -> new Pattern.Bind();
            case 2 -> new Pattern.Literal(readConstValue());
            case 3 -> {
                int preN = readLen();
                List<Pattern> prefix = new ArrayList<>(preN);
                for (int i = 0; i < preN; i++) {
                    prefix.add(readPattern());
                }
                int restTag = readU8();
                Pattern rest =
                        switch (restTag) {
                            case 0 -> null;
                            case 1 -> readPattern();
                            default -> throw err("invalid option tag " + restTag);
                        };
                int sufN = readLen();
                List<Pattern> suffix = new ArrayList<>(sufN);
                for (int i = 0; i < sufN; i++) {
                    suffix.add(readPattern());
                }
                yield new Pattern.Tuple(prefix, rest, suffix);
            }
            case 4 -> {
                String enumName = readString();
                String variant = readString();
                int n = readLen();
                List<Pattern> fields = new ArrayList<>(n);
                for (int i = 0; i < n; i++) {
                    fields.add(readPattern());
                }
                yield new Pattern.Enum(enumName, variant, fields);
            }
            case 5 -> {
                String typeName = readString();
                int n = readLen();
                List<Pattern.Struct.Field> fields = new ArrayList<>(n);
                for (int i = 0; i < n; i++) {
                    String name = readString();
                    Pattern p = readPattern();
                    fields.add(new Pattern.Struct.Field(name, p));
                }
                yield new Pattern.Struct(typeName, fields);
            }
            case 6 -> {
                int preN = readLen();
                List<Pattern> prefix = new ArrayList<>(preN);
                for (int i = 0; i < preN; i++) {
                    prefix.add(readPattern());
                }
                int restTag = readU8();
                Pattern rest =
                        switch (restTag) {
                            case 0 -> null;
                            case 1 -> readPattern();
                            default -> throw err("invalid option tag " + restTag);
                        };
                int sufN = readLen();
                List<Pattern> suffix = new ArrayList<>(sufN);
                for (int i = 0; i < sufN; i++) {
                    suffix.add(readPattern());
                }
                yield new Pattern.Array(prefix, rest, suffix);
            }
            default -> throw err("invalid Pattern tag " + tag);
        };
    }

    private EffectSpec readEffectSpec() throws RbcDecodeException {
        String iface = readString();
        List<Integer> ifaceArgs = readVecReg();
        String method = readString();
        return new EffectSpec(iface, ifaceArgs, method);
    }

    private HandlerClause readHandlerClause() throws RbcDecodeException {
        EffectSpec effect = readEffectSpec();
        int n = readLen();
        List<Pattern> argPatterns = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            argPatterns.add(readPattern());
        }
        long targetPc = readU32();
        if (targetPc > Integer.MAX_VALUE) {
            throw err("pc overflow");
        }
        List<Integer> paramRegs = readVecReg();
        return new HandlerClause(effect, argPatterns, (int) targetPc, paramRegs);
    }

    private SwitchCase readSwitchCase() throws RbcDecodeException {
        Pattern pat = readPattern();
        long targetPc = readU32();
        if (targetPc > Integer.MAX_VALUE) {
            throw err("pc overflow");
        }
        List<Integer> paramRegs = readVecReg();
        return new SwitchCase(pat, (int) targetPc, paramRegs);
    }

    private Intrinsic readIntrinsic() throws RbcDecodeException {
        int tag = readU16();
        try {
            return Intrinsic.fromTag(tag);
        } catch (IllegalArgumentException e) {
            throw err("invalid Intrinsic tag " + tag);
        }
    }

    private CallTarget readCallTarget() throws RbcDecodeException {
        int tag = readU8();
        return switch (tag) {
            case 0 -> {
                long id = readU32();
                if (id > Integer.MAX_VALUE) {
                    throw err("function id overflow");
                }
                yield new CallTarget.Bc(new FunctionId((int) id));
            }
            case 1 -> {
                long id = readU32();
                if (id > Integer.MAX_VALUE) {
                    throw err("host import id overflow");
                }
                yield new CallTarget.Host(new HostImportId((int) id));
            }
            case 2 -> new CallTarget.IntrinsicTarget(readIntrinsic());
            default -> throw err("invalid CallTarget tag " + tag);
        };
    }

    private Instruction readInstruction() throws RbcDecodeException {
        int opcode = readU8();
        return switch (opcode) {
            case 0 -> new Instruction.Const((int) readU32Checked("reg overflow"), readConstValue());
            case 1 -> new Instruction.Copy(readU32Reg(), readU32Reg());
            case 2 -> new Instruction.Move(readU32Reg(), readU32Reg());
            case 3 -> new Instruction.AsReadonly(readU32Reg(), readU32Reg());
            case 4 -> new Instruction.IsType(readU32Reg(), readU32Reg(), readU32Reg());
            case 5 -> new Instruction.MakeTypeRep(readU32Reg(), readTypeRepLit(), readVecReg());
            case 62 -> new Instruction.AssocTypeRep(
                    readU32Reg(), readU32Reg(), new TypeId(readU32Usize("type id overflow")), readString());
            case 6 -> {
                int dst = readU32Reg();
                long typeId = readU32();
                if (typeId > Integer.MAX_VALUE) {
                    throw err("type id overflow");
                }
                List<Integer> typeArgs = readVecReg();
                int n = readLen();
                List<Instruction.MakeStruct.FieldInit> fields = new ArrayList<>(n);
                for (int i = 0; i < n; i++) {
                    String name = readString();
                    int reg = readU32Reg();
                    fields.add(new Instruction.MakeStruct.FieldInit(name, reg));
                }
                yield new Instruction.MakeStruct(dst, new TypeId((int) typeId), typeArgs, fields);
            }
            case 7 -> new Instruction.MakeArray(readU32Reg(), readVecReg());
            case 8 -> new Instruction.MakeTuple(readU32Reg(), readVecReg());
            case 9 -> {
                int dst = readU32Reg();
                long typeId = readU32();
                if (typeId > Integer.MAX_VALUE) {
                    throw err("type id overflow");
                }
                List<Integer> typeArgs = readVecReg();
                String variant = readString();
                List<Integer> fields = readVecReg();
                yield new Instruction.MakeEnum(dst, new TypeId((int) typeId), typeArgs, variant, fields);
            }
            case 10 -> new Instruction.GetField(readU32Reg(), readU32Reg(), readString());
            case 11 -> new Instruction.SetField(readU32Reg(), readString(), readU32Reg());
            case 12 -> new Instruction.StructGet(readU32Reg(), readU32Reg(), readU32Usize("struct field index overflow"));
            case 13 -> new Instruction.StructSet(readU32Reg(), readU32Usize("struct field index overflow"), readU32Reg());
            case 14 -> new Instruction.TupleGet(readU32Reg(), readU32Reg(), readU32Usize("tuple index overflow"));
            case 15 -> new Instruction.TupleSet(readU32Reg(), readU32Usize("tuple index overflow"), readU32Reg());
            case 16 -> new Instruction.IndexGet(readU32Reg(), readU32Reg(), readU32Reg());
            case 17 -> new Instruction.IndexSet(readU32Reg(), readU32Reg(), readU32Reg());
            case 18 -> new Instruction.Len(readU32Reg(), readU32Reg());
            case 19 -> new Instruction.IntAdd(readU32Reg(), readU32Reg(), readU32Reg());
            case 20 -> new Instruction.IntSub(readU32Reg(), readU32Reg(), readU32Reg());
            case 21 -> new Instruction.IntMul(readU32Reg(), readU32Reg(), readU32Reg());
            case 22 -> new Instruction.IntDiv(readU32Reg(), readU32Reg(), readU32Reg());
            case 23 -> new Instruction.IntMod(readU32Reg(), readU32Reg(), readU32Reg());
            case 48 -> new Instruction.IntAnd(readU32Reg(), readU32Reg(), readU32Reg());
            case 49 -> new Instruction.IntOr(readU32Reg(), readU32Reg(), readU32Reg());
            case 50 -> new Instruction.IntXor(readU32Reg(), readU32Reg(), readU32Reg());
            case 51 -> new Instruction.IntShl(readU32Reg(), readU32Reg(), readU32Reg());
            case 52 -> new Instruction.IntShr(readU32Reg(), readU32Reg(), readU32Reg());
            case 53 -> new Instruction.IntUShr(readU32Reg(), readU32Reg(), readU32Reg());
            case 54 -> new Instruction.IntNot(readU32Reg(), readU32Reg());
            case 24 -> new Instruction.IntLt(readU32Reg(), readU32Reg(), readU32Reg());
            case 25 -> new Instruction.IntLe(readU32Reg(), readU32Reg(), readU32Reg());
            case 26 -> new Instruction.IntGt(readU32Reg(), readU32Reg(), readU32Reg());
            case 27 -> new Instruction.IntGe(readU32Reg(), readU32Reg(), readU32Reg());
            case 28 -> new Instruction.IntEq(readU32Reg(), readU32Reg(), readU32Reg());
            case 29 -> new Instruction.IntNe(readU32Reg(), readU32Reg(), readU32Reg());
            case 55 -> new Instruction.ByteAnd(readU32Reg(), readU32Reg(), readU32Reg());
            case 56 -> new Instruction.ByteOr(readU32Reg(), readU32Reg(), readU32Reg());
            case 57 -> new Instruction.ByteXor(readU32Reg(), readU32Reg(), readU32Reg());
            case 58 -> new Instruction.ByteShl(readU32Reg(), readU32Reg(), readU32Reg());
            case 59 -> new Instruction.ByteShr(readU32Reg(), readU32Reg(), readU32Reg());
            case 60 -> new Instruction.ByteUShr(readU32Reg(), readU32Reg(), readU32Reg());
            case 61 -> new Instruction.ByteNot(readU32Reg(), readU32Reg());
            case 30 -> new Instruction.BoolNot(readU32Reg(), readU32Reg());
            case 31 -> new Instruction.BoolEq(readU32Reg(), readU32Reg(), readU32Reg());
            case 32 -> new Instruction.BoolNe(readU32Reg(), readU32Reg(), readU32Reg());
            case 33 -> new Instruction.Call(readOptionReg(), readCallTarget(), readVecReg());
            case 46 -> new Instruction.CallMulti(readVecReg(), readCallTarget(), readVecReg());
            case 34 -> new Instruction.ICall(readOptionReg(), readU32Reg(), readVecReg());
            case 35 -> {
                Integer dst = readOptionReg();
                int obj = readU32Reg();
                long methodId = readU32();
                if (methodId > Integer.MAX_VALUE) {
                    throw err("method id overflow");
                }
                List<Integer> methodTypeArgs = readVecReg();
                List<Integer> args = readVecReg();
                yield new Instruction.VCall(dst, obj, new MethodId((int) methodId), methodTypeArgs, args);
            }
            case 36 -> {
                int n = readLen();
                List<HandlerClause> clauses = new ArrayList<>(n);
                for (int i = 0; i < n; i++) {
                    clauses.add(readHandlerClause());
                }
                yield new Instruction.PushHandler(clauses);
            }
            case 37 -> new Instruction.PopHandler();
            case 38 -> new Instruction.Perform(readOptionReg(), readEffectSpec(), readVecReg());
            case 39 -> new Instruction.Resume(readOptionReg(), readU32Reg(), readU32Reg());
            case 45 -> new Instruction.ResumeTail(readU32Reg(), readU32Reg());
            case 40 -> new Instruction.Jump((int) readU32Checked("pc overflow"));
            case 41 -> new Instruction.JumpIf(readU32Reg(), (int) readU32Checked("pc overflow"), (int) readU32Checked("pc overflow"));
            case 42 -> {
                int value = readU32Reg();
                int n = readLen();
                List<SwitchCase> cases = new ArrayList<>(n);
                for (int i = 0; i < n; i++) {
                    cases.add(readSwitchCase());
                }
                long defaultPc = readU32();
                if (defaultPc > Integer.MAX_VALUE) {
                    throw err("pc overflow");
                }
                yield new Instruction.Switch(value, cases, (int) defaultPc);
            }
            case 43 -> new Instruction.Return(readU32Reg());
            case 47 -> new Instruction.ReturnMulti(readVecReg());
            case 44 -> new Instruction.Trap(readString());
            default -> throw err("invalid Instruction opcode " + opcode);
        };
    }

    private int readU32Reg() throws RbcDecodeException {
        long v = readU32();
        if (v > Integer.MAX_VALUE) {
            throw err("reg overflow");
        }
        return (int) v;
    }

    private int readU32Usize(String overflowMessage) throws RbcDecodeException {
        long v = readU32();
        if (v > Integer.MAX_VALUE) {
            throw err(overflowMessage);
        }
        return (int) v;
    }

    private long readU32Checked(String message) throws RbcDecodeException {
        long v = readU32();
        if (v > Integer.MAX_VALUE) {
            throw err(message);
        }
        return v;
    }
}
