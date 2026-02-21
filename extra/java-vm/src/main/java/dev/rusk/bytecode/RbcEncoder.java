package dev.rusk.bytecode;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

final class RbcEncoder {
    private static final byte[] MAGIC = new byte[] {'R', 'U', 'S', 'K', 'B', 'C', '0', 0};
    private static final int VERSION_MAJOR = 0;
    private static final int VERSION_MINOR = 11;

    private final ByteArrayOutputStream out = new ByteArrayOutputStream();

    byte[] encode(ExecutableModule module) throws RbcEncodeException {
        writeBytes(MAGIC);
        writeU16(VERSION_MAJOR);
        writeU16(VERSION_MINOR);
        writeModule(module);
        return out.toByteArray();
    }

    private void writeBytes(byte[] bytes) {
        out.writeBytes(bytes);
    }

    private void writeU8(int v) {
        out.write(v & 0xFF);
    }

    private void writeU16(int v) {
        writeU8(v);
        writeU8(v >>> 8);
    }

    private void writeU32(long v) throws RbcEncodeException {
        if (v < 0 || v > 0xFFFF_FFFFL) {
            throw new RbcEncodeException("u32 overflow");
        }
        writeU8((int) (v));
        writeU8((int) (v >>> 8));
        writeU8((int) (v >>> 16));
        writeU8((int) (v >>> 24));
    }

    private void writeU64(long v) {
        for (int i = 0; i < 8; i++) {
            writeU8((int) (v >>> (8 * i)));
        }
    }

    private void writeI64(long v) {
        writeU64(v);
    }

    private void writeF64(double v) {
        writeU64(Double.doubleToLongBits(v));
    }

    private void writeBool(boolean v) {
        writeU8(v ? 1 : 0);
    }

    private void writeLen(int len) throws RbcEncodeException {
        if (len < 0) {
            throw new RbcEncodeException("length overflow");
        }
        writeU32(len);
    }

    private void writeString(String s) throws RbcEncodeException {
        byte[] b = s.getBytes(StandardCharsets.UTF_8);
        writeLen(b.length);
        writeBytes(b);
    }

    private void writeBlob(byte[] b) throws RbcEncodeException {
        writeLen(b.length);
        writeBytes(b);
    }

    private void writeVecReg(List<Integer> regs) throws RbcEncodeException {
        writeLen(regs.size());
        for (int r : regs) {
            writeU32(r);
        }
    }

    private void writeOptionReg(Integer reg) throws RbcEncodeException {
        if (reg == null) {
            writeU8(0);
        } else {
            writeU8(1);
            writeU32(reg);
        }
    }

    private void writeModule(ExecutableModule module) throws RbcEncodeException {
        // Functions
        writeLen(module.functions().size());
        for (Function f : module.functions()) {
            writeFunction(f);
        }

        // Generic params table
        writeLen(module.functionGenericParams().size());
        for (int n : module.functionGenericParams()) {
            writeU32(n);
        }

        // Host imports
        writeLen(module.hostImports().size());
        for (HostImport imp : module.hostImports()) {
            writeHostImport(imp);
        }

        // Interned type names.
        writeLen(module.typeNames().size());
        for (String name : module.typeNames()) {
            writeString(name);
        }

        // Interned method names.
        writeLen(module.methodNames().size());
        for (String name : module.methodNames()) {
            writeString(name);
        }

        // VCall dispatch table (sparse encoding, deterministic order by type_id then method_id).
        int dispatchLen = 0;
        for (List<ExecutableModule.VCallEntry> entries : module.vcallDispatch()) {
            dispatchLen += entries.size();
        }
        writeLen(dispatchLen);
        for (int typeIndex = 0; typeIndex < module.vcallDispatch().size(); typeIndex++) {
            for (ExecutableModule.VCallEntry e : module.vcallDispatch().get(typeIndex)) {
                writeU32(typeIndex);
                writeU32(e.method().index());
                writeU32(e.function().index());
            }
        }

        // Interface impls (sparse encoding).
        int implLen = 0;
        for (List<TypeId> ifaces : module.interfaceImpls()) {
            if (!ifaces.isEmpty()) {
                implLen += 1;
            }
        }
        writeLen(implLen);
        for (int typeIndex = 0; typeIndex < module.interfaceImpls().size(); typeIndex++) {
            List<TypeId> ifaces = module.interfaceImpls().get(typeIndex);
            if (ifaces.isEmpty()) {
                continue;
            }
            writeU32(typeIndex);
            writeLen(ifaces.size());
            for (TypeId ifaceId : ifaces) {
                writeU32(ifaceId.index());
            }
        }

        // Struct layouts (sparse encoding).
        int layoutLen = 0;
        for (List<String> layout : module.structLayouts()) {
            if (layout != null) {
                layoutLen += 1;
            }
        }
        writeLen(layoutLen);
        for (int typeIndex = 0; typeIndex < module.structLayouts().size(); typeIndex++) {
            List<String> fields = module.structLayouts().get(typeIndex);
            if (fields == null) {
                continue;
            }
            writeU32(typeIndex);
            writeLen(fields.size());
            for (String field : fields) {
                writeString(field);
            }
        }

        // External effects.
        writeLen(module.externalEffects().size());
        for (ExternalEffectDecl decl : module.externalEffects()) {
            writeExternalEffectDecl(decl);
        }

        // Entry
        writeU32(module.entry().index());
    }

    private void writeFunction(Function func) throws RbcEncodeException {
        writeString(func.name());
        writeU32(func.regCount());
        writeU32(func.paramCount());
        writeLen(func.code().size());
        for (Instruction inst : func.code()) {
            writeInstruction(inst);
        }
    }

    private void writeAbiType(AbiType ty) {
        writeU8(ty.tag());
    }

    private void writeHostFnSig(HostFnSig sig) throws RbcEncodeException {
        writeLen(sig.params().size());
        for (AbiType ty : sig.params()) {
            writeAbiType(ty);
        }
        writeAbiType(sig.ret());
    }

    private void writeHostImport(HostImport imp) throws RbcEncodeException {
        writeString(imp.name());
        writeHostFnSig(imp.sig());
    }

    private void writeExternalEffectDecl(ExternalEffectDecl decl) throws RbcEncodeException {
        writeString(decl.interfaceName());
        writeString(decl.method());
        writeHostFnSig(decl.sig());
    }

    private void writeConstValue(ConstValue v) throws RbcEncodeException {
        if (v instanceof ConstValue.Unit) {
            writeU8(0);
        } else if (v instanceof ConstValue.Bool b) {
            writeU8(1);
            writeBool(b.value());
        } else if (v instanceof ConstValue.Int n) {
            writeU8(2);
            writeI64(n.value());
        } else if (v instanceof ConstValue.Float x) {
            writeU8(3);
            writeF64(x.value());
        } else if (v instanceof ConstValue.Str s) {
            writeU8(4);
            writeString(s.value());
        } else if (v instanceof ConstValue.Bytes b) {
            writeU8(5);
            writeBlob(b.value());
        } else if (v instanceof ConstValue.TypeRep t) {
            writeU8(6);
            writeTypeRepLit(t.value());
        } else if (v instanceof ConstValue.Function f) {
            writeU8(7);
            writeU32(f.value().index());
        } else {
            throw new RbcEncodeException("unknown ConstValue");
        }
    }

    private void writeTypeRepLit(TypeRepLit lit) throws RbcEncodeException {
        if (lit instanceof TypeRepLit.Unit) {
            writeU8(0);
        } else if (lit instanceof TypeRepLit.Never) {
            writeU8(1);
        } else if (lit instanceof TypeRepLit.Bool) {
            writeU8(2);
        } else if (lit instanceof TypeRepLit.Int) {
            writeU8(3);
        } else if (lit instanceof TypeRepLit.Float) {
            writeU8(4);
        } else if (lit instanceof TypeRepLit.Byte) {
            writeU8(5);
        } else if (lit instanceof TypeRepLit.Char) {
            writeU8(6);
        } else if (lit instanceof TypeRepLit.String) {
            writeU8(7);
        } else if (lit instanceof TypeRepLit.Bytes) {
            writeU8(8);
        } else if (lit instanceof TypeRepLit.Array) {
            writeU8(9);
        } else if (lit instanceof TypeRepLit.Tuple t) {
            writeU8(10);
            writeU32(t.arity());
        } else if (lit instanceof TypeRepLit.Struct s) {
            writeU8(11);
            writeString(s.name());
        } else if (lit instanceof TypeRepLit.Enum e) {
            writeU8(12);
            writeString(e.name());
        } else if (lit instanceof TypeRepLit.Interface i) {
            writeU8(13);
            writeString(i.name());
        } else if (lit instanceof TypeRepLit.Fn) {
            writeU8(14);
        } else if (lit instanceof TypeRepLit.Cont) {
            writeU8(15);
        } else {
            throw new RbcEncodeException("unknown TypeRepLit");
        }
    }

    private void writePattern(Pattern pat) throws RbcEncodeException {
        if (pat instanceof Pattern.Wildcard) {
            writeU8(0);
        } else if (pat instanceof Pattern.Bind) {
            writeU8(1);
        } else if (pat instanceof Pattern.Literal lit) {
            writeU8(2);
            writeConstValue(lit.value());
        } else if (pat instanceof Pattern.Tuple tup) {
            writeU8(3);
            writeLen(tup.prefix().size());
            for (Pattern p : tup.prefix()) {
                writePattern(p);
            }
            if (tup.rest() == null) {
                writeU8(0);
            } else {
                writeU8(1);
                writePattern(tup.rest());
            }
            writeLen(tup.suffix().size());
            for (Pattern p : tup.suffix()) {
                writePattern(p);
            }
        } else if (pat instanceof Pattern.Enum e) {
            writeU8(4);
            writeString(e.enumName());
            writeString(e.variant());
            writeLen(e.fields().size());
            for (Pattern p : e.fields()) {
                writePattern(p);
            }
        } else if (pat instanceof Pattern.Struct s) {
            writeU8(5);
            writeString(s.typeName());
            writeLen(s.fields().size());
            for (Pattern.Struct.Field f : s.fields()) {
                writeString(f.name());
                writePattern(f.pattern());
            }
        } else if (pat instanceof Pattern.Array a) {
            writeU8(6);
            writeLen(a.prefix().size());
            for (Pattern p : a.prefix()) {
                writePattern(p);
            }
            if (a.rest() == null) {
                writeU8(0);
            } else {
                writeU8(1);
                writePattern(a.rest());
            }
            writeLen(a.suffix().size());
            for (Pattern p : a.suffix()) {
                writePattern(p);
            }
        } else {
            throw new RbcEncodeException("unknown Pattern");
        }
    }

    private void writeEffectSpec(EffectSpec spec) throws RbcEncodeException {
        writeString(spec.interfaceName());
        writeVecReg(spec.interfaceArgs());
        writeString(spec.method());
    }

    private void writeHandlerClause(HandlerClause clause) throws RbcEncodeException {
        writeEffectSpec(clause.effect());
        writeLen(clause.argPatterns().size());
        for (Pattern p : clause.argPatterns()) {
            writePattern(p);
        }
        writeU32(clause.targetPc());
        writeVecReg(clause.paramRegs());
    }

    private void writeSwitchCase(SwitchCase c) throws RbcEncodeException {
        writePattern(c.pattern());
        writeU32(c.targetPc());
        writeVecReg(c.paramRegs());
    }

    private void writeIntrinsic(Intrinsic intr) {
        writeU16(intr.tag());
    }

    private void writeCallTarget(CallTarget target) throws RbcEncodeException {
        if (target instanceof CallTarget.Bc bc) {
            writeU8(0);
            writeU32(bc.function().index());
        } else if (target instanceof CallTarget.Host host) {
            writeU8(1);
            writeU32(host.hostImport().index());
        } else if (target instanceof CallTarget.IntrinsicTarget intr) {
            writeU8(2);
            writeIntrinsic(intr.intrinsic());
        } else {
            throw new RbcEncodeException("unknown CallTarget");
        }
    }

    private void writeInstruction(Instruction inst) throws RbcEncodeException {
        if (inst instanceof Instruction.Const i) {
            writeU8(0);
            writeU32(i.dst());
            writeConstValue(i.value());
        } else if (inst instanceof Instruction.Copy i) {
            writeU8(1);
            writeU32(i.dst());
            writeU32(i.src());
        } else if (inst instanceof Instruction.Move i) {
            writeU8(2);
            writeU32(i.dst());
            writeU32(i.src());
        } else if (inst instanceof Instruction.AsReadonly i) {
            writeU8(3);
            writeU32(i.dst());
            writeU32(i.src());
        } else if (inst instanceof Instruction.IsType i) {
            writeU8(4);
            writeU32(i.dst());
            writeU32(i.value());
            writeU32(i.ty());
        } else if (inst instanceof Instruction.MakeTypeRep i) {
            writeU8(5);
            writeU32(i.dst());
            writeTypeRepLit(i.base());
            writeVecReg(i.args());
        } else if (inst instanceof Instruction.MakeStruct i) {
            writeU8(6);
            writeU32(i.dst());
            writeU32(i.typeId().index());
            writeVecReg(i.typeArgs());
            writeLen(i.fields().size());
            for (Instruction.MakeStruct.FieldInit f : i.fields()) {
                writeString(f.name());
                writeU32(f.reg());
            }
        } else if (inst instanceof Instruction.MakeArray i) {
            writeU8(7);
            writeU32(i.dst());
            writeVecReg(i.items());
        } else if (inst instanceof Instruction.MakeTuple i) {
            writeU8(8);
            writeU32(i.dst());
            writeVecReg(i.items());
        } else if (inst instanceof Instruction.MakeEnum i) {
            writeU8(9);
            writeU32(i.dst());
            writeU32(i.enumTypeId().index());
            writeVecReg(i.typeArgs());
            writeString(i.variant());
            writeVecReg(i.fields());
        } else if (inst instanceof Instruction.GetField i) {
            writeU8(10);
            writeU32(i.dst());
            writeU32(i.obj());
            writeString(i.field());
        } else if (inst instanceof Instruction.SetField i) {
            writeU8(11);
            writeU32(i.obj());
            writeString(i.field());
            writeU32(i.value());
        } else if (inst instanceof Instruction.StructGet i) {
            writeU8(12);
            writeU32(i.dst());
            writeU32(i.obj());
            writeU32(i.idx());
        } else if (inst instanceof Instruction.StructSet i) {
            writeU8(13);
            writeU32(i.obj());
            writeU32(i.idx());
            writeU32(i.value());
        } else if (inst instanceof Instruction.TupleGet i) {
            writeU8(14);
            writeU32(i.dst());
            writeU32(i.tup());
            writeU32(i.idx());
        } else if (inst instanceof Instruction.TupleSet i) {
            writeU8(15);
            writeU32(i.tup());
            writeU32(i.idx());
            writeU32(i.value());
        } else if (inst instanceof Instruction.IndexGet i) {
            writeU8(16);
            writeU32(i.dst());
            writeU32(i.arr());
            writeU32(i.idx());
        } else if (inst instanceof Instruction.IndexSet i) {
            writeU8(17);
            writeU32(i.arr());
            writeU32(i.idx());
            writeU32(i.value());
        } else if (inst instanceof Instruction.Len i) {
            writeU8(18);
            writeU32(i.dst());
            writeU32(i.arr());
        } else if (inst instanceof Instruction.IntAdd i) {
            writeU8(19);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntSub i) {
            writeU8(20);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntMul i) {
            writeU8(21);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntDiv i) {
            writeU8(22);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntMod i) {
            writeU8(23);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntAnd i) {
            writeU8(48);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntOr i) {
            writeU8(49);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntXor i) {
            writeU8(50);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntShl i) {
            writeU8(51);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntShr i) {
            writeU8(52);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntUShr i) {
            writeU8(53);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntNot i) {
            writeU8(54);
            writeU32(i.dst());
            writeU32(i.v());
        } else if (inst instanceof Instruction.IntLt i) {
            writeU8(24);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntLe i) {
            writeU8(25);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntGt i) {
            writeU8(26);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntGe i) {
            writeU8(27);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntEq i) {
            writeU8(28);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.IntNe i) {
            writeU8(29);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.ByteAnd i) {
            writeU8(55);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.ByteOr i) {
            writeU8(56);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.ByteXor i) {
            writeU8(57);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.ByteShl i) {
            writeU8(58);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.ByteShr i) {
            writeU8(59);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.ByteUShr i) {
            writeU8(60);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.ByteNot i) {
            writeU8(61);
            writeU32(i.dst());
            writeU32(i.v());
        } else if (inst instanceof Instruction.BoolNot i) {
            writeU8(30);
            writeU32(i.dst());
            writeU32(i.v());
        } else if (inst instanceof Instruction.BoolEq i) {
            writeU8(31);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.BoolNe i) {
            writeU8(32);
            writeU32(i.dst());
            writeU32(i.a());
            writeU32(i.b());
        } else if (inst instanceof Instruction.Call i) {
            writeU8(33);
            writeOptionReg(i.dst());
            writeCallTarget(i.func());
            writeVecReg(i.args());
        } else if (inst instanceof Instruction.ICall i) {
            writeU8(34);
            writeOptionReg(i.dst());
            writeU32(i.fnptr());
            writeVecReg(i.args());
        } else if (inst instanceof Instruction.VCall i) {
            writeU8(35);
            writeOptionReg(i.dst());
            writeU32(i.obj());
            writeU32(i.method().index());
            writeVecReg(i.methodTypeArgs());
            writeVecReg(i.args());
        } else if (inst instanceof Instruction.PushHandler i) {
            writeU8(36);
            writeLen(i.clauses().size());
            for (HandlerClause c : i.clauses()) {
                writeHandlerClause(c);
            }
        } else if (inst instanceof Instruction.PopHandler) {
            writeU8(37);
        } else if (inst instanceof Instruction.Perform i) {
            writeU8(38);
            writeOptionReg(i.dst());
            writeEffectSpec(i.effect());
            writeVecReg(i.args());
        } else if (inst instanceof Instruction.Resume i) {
            writeU8(39);
            writeOptionReg(i.dst());
            writeU32(i.k());
            writeU32(i.value());
        } else if (inst instanceof Instruction.ResumeTail i) {
            writeU8(45);
            writeU32(i.k());
            writeU32(i.value());
        } else if (inst instanceof Instruction.Jump i) {
            writeU8(40);
            writeU32(i.targetPc());
        } else if (inst instanceof Instruction.JumpIf i) {
            writeU8(41);
            writeU32(i.cond());
            writeU32(i.thenPc());
            writeU32(i.elsePc());
        } else if (inst instanceof Instruction.Switch i) {
            writeU8(42);
            writeU32(i.value());
            writeLen(i.cases().size());
            for (SwitchCase c : i.cases()) {
                writeSwitchCase(c);
            }
            writeU32(i.defaultPc());
        } else if (inst instanceof Instruction.Return i) {
            writeU8(43);
            writeU32(i.value());
        } else if (inst instanceof Instruction.Trap i) {
            writeU8(44);
            writeString(i.message());
        } else if (inst instanceof Instruction.CallMulti i) {
            writeU8(46);
            writeVecReg(i.dsts());
            writeCallTarget(i.func());
            writeVecReg(i.args());
        } else if (inst instanceof Instruction.ReturnMulti i) {
            writeU8(47);
            writeVecReg(i.values());
        } else {
            throw new RbcEncodeException("unknown Instruction");
        }
    }
}
