package dev.rusk.vm;

import dev.rusk.bytecode.AbiType;
import dev.rusk.bytecode.CallTarget;
import dev.rusk.bytecode.ConstValue;
import dev.rusk.bytecode.EffectId;
import dev.rusk.bytecode.EffectSpec;
import dev.rusk.bytecode.ExecutableModule;
import dev.rusk.bytecode.ExternalEffectDecl;
import dev.rusk.bytecode.Function;
import dev.rusk.bytecode.FunctionId;
import dev.rusk.bytecode.HandlerClause;
import dev.rusk.bytecode.HostImport;
import dev.rusk.bytecode.HostImportId;
import dev.rusk.bytecode.Instruction;
import dev.rusk.bytecode.Intrinsic;
import dev.rusk.bytecode.MethodId;
import dev.rusk.bytecode.Pattern;
import dev.rusk.bytecode.SwitchCase;
import dev.rusk.bytecode.TypeId;
import dev.rusk.bytecode.TypeRepLit;
import java.nio.charset.CharacterCodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Java 版 Rusk Bytecode VM（解释器）。
 *
 * <p>宿主通过 {@link #step(Long)} 驱动执行；当遇到外部化 effect 时返回 {@link StepResult.Request}，
 * 宿主再通过 {@link #resume(ContinuationHandle, AbiValue)} 或 {@link #dropContinuation(ContinuationHandle)}
 * 恢复/取消。</p>
 */
public final class Vm {
    private record PrimitiveTypeIds(
            TypeId unit,
            TypeId bool,
            TypeId intTy,
            TypeId floatTy,
            TypeId byteTy,
            TypeId charTy,
            TypeId stringTy,
            TypeId bytesTy) {}

    private record VCallFastPathIds(MethodId hash, MethodId eq, MethodId ne) {}

    private final ExecutableModule module;
    private final HostFn[] hostFns;

    private final PrimitiveTypeIds primitiveTypeIds;
    private final VCallFastPathIds vcallFastPathIds;

    private final TypeReps typeReps = new TypeReps();
    private final ArrayList<Frame> frames = new ArrayList<>();
    private final ArrayList<HandlerEntry> handlers = new ArrayList<>();
    private final PinnedContinuations pinnedContinuations = new PinnedContinuations();

    private VmState state = new VmState.Running();
    private boolean inHostCall = false;
    private int continuationGeneration = 0;

    public Vm(ExecutableModule module, HostImportRegistry hostImports) throws VmError {
        this(module, hostImports, null);
    }

    /**
     * 运行 entry(main) 并注入 argv（当 entry 期望 1 个参数时）。
     *
     * <p>对齐 Rust 参考 VM：若传入空 argv，会自动补一个空字符串，保证 argv 至少有 1 个元素。</p>
     */
    public Vm(ExecutableModule module, HostImportRegistry hostImports, List<String> argv) throws VmError {
        this.module = Objects.requireNonNull(module, "module");
        Objects.requireNonNull(hostImports, "hostImports");

        // Host imports preflight: every declared import must be installed.
        this.hostFns = new HostFn[module.hostImports().size()];
        for (int i = 0; i < module.hostImports().size(); i++) {
            HostImport imp = module.hostImports().get(i);
            HostFn fn = hostImports.resolve(i, imp.name());
            if (fn == null) {
                throw new VmError.InvalidState("missing host import implementation: `" + imp.name() + "`");
            }
            hostFns[i] = fn;
        }

        this.primitiveTypeIds = primitiveTypeIdsFromModule(module);
        this.vcallFastPathIds =
                new VCallFastPathIds(
                        module.methodId("core::hash::Hash::hash").orElse(null),
                        module.methodId("core::ops::Eq::eq").orElse(null),
                        module.methodId("core::ops::Ne::ne").orElse(null));

        FunctionId entry = module.entry();
        Function entryFn =
                module.function(entry).orElseThrow(() -> new VmError.InvalidState("invalid entry function id " + entry.index()));

        if (entry.index() < 0 || entry.index() >= module.functionGenericParams().size()) {
            throw new VmError.InvalidState("entry function id out of range: " + entry.index());
        }
        int entryGenericParams = module.functionGenericParams().get(entry.index());
        if (entryGenericParams != 0) {
            throw new VmError.InvalidState("entry function cannot be generic");
        }

        if (entryFn.paramCount() > entryFn.regCount()) {
            throw new VmError.InvalidState(
                    "entry param_count " + entryFn.paramCount() + " exceeds reg_count " + entryFn.regCount());
        }

        if (argv == null) {
            if (entryFn.paramCount() != 0) {
                throw new VmError.InvalidState(
                        "entry function expects " + entryFn.paramCount() + " parameter(s); pass argv to Vm constructor");
            }
        } else {
            if (entryFn.paramCount() != 1) {
                throw new VmError.InvalidState(
                        "entry function expects " + entryFn.paramCount() + " parameter(s); cannot pass argv");
            }
        }

        Value[] regs = new Value[entryFn.regCount()];
        if (argv != null) {
            ArrayList<String> argvList = new ArrayList<>(argv);
            if (argvList.isEmpty()) {
                argvList.add("");
            }
            ArrayList<Value> items = new ArrayList<>(argvList.size());
            for (String s : argvList) {
                items.add(new Value.Str(new RuskString(s)));
            }
            Value argvValue = allocRef(new ArrayObj(items));
            if (regs.length < 1) {
                throw new VmError.InvalidState("entry param reg 0 out of range");
            }
            regs[0] = argvValue;
        }
        frames.add(new Frame(entry, 0, regs, new ReturnDsts.None()));
    }

    private static PrimitiveTypeIds primitiveTypeIdsFromModule(ExecutableModule module) throws VmError {
        TypeId unit =
                module.typeId("unit")
                        .orElseThrow(
                                () ->
                                        new VmError.InvalidState(
                                                "missing required primitive type name `unit` in module type table"));
        TypeId bool =
                module.typeId("bool")
                        .orElseThrow(
                                () ->
                                        new VmError.InvalidState(
                                                "missing required primitive type name `bool` in module type table"));
        TypeId intTy =
                module.typeId("int")
                        .orElseThrow(
                                () ->
                                        new VmError.InvalidState(
                                                "missing required primitive type name `int` in module type table"));
        TypeId floatTy =
                module.typeId("float")
                        .orElseThrow(
                                () ->
                                        new VmError.InvalidState(
                                                "missing required primitive type name `float` in module type table"));
        TypeId byteTy =
                module.typeId("byte")
                        .orElseThrow(
                                () ->
                                        new VmError.InvalidState(
                                                "missing required primitive type name `byte` in module type table"));
        TypeId charTy =
                module.typeId("char")
                        .orElseThrow(
                                () ->
                                        new VmError.InvalidState(
                                                "missing required primitive type name `char` in module type table"));
        TypeId stringTy =
                module.typeId("string")
                        .orElseThrow(
                                () ->
                                        new VmError.InvalidState(
                                                "missing required primitive type name `string` in module type table"));
        TypeId bytesTy =
                module.typeId("bytes")
                        .orElseThrow(
                                () ->
                                        new VmError.InvalidState(
                                                "missing required primitive type name `bytes` in module type table"));
        return new PrimitiveTypeIds(unit, bool, intTy, floatTy, byteTy, charTy, stringTy, bytesTy);
    }

    public StepResult step(Long fuel) {
        if (inHostCall) {
            return trap("vm re-entered during host call");
        }

        if (state instanceof VmState.Suspended) {
            return new StepResult.Trap("vm is suspended; call resume/drop first");
        }
        if (state instanceof VmState.Done done) {
            return new StepResult.Done(done.value);
        }
        if (state instanceof VmState.Trapped trapped) {
            return new StepResult.Trap(trapped.message);
        }

        long remaining = (fuel == null) ? Long.MAX_VALUE : fuel.longValue();

        while (true) {
            if (fuel != null && remaining == 0) {
                return new StepResult.Yield(0);
            }

            if (frames.isEmpty()) {
                state = new VmState.Done(new AbiValue.Unit());
                return new StepResult.Done(new AbiValue.Unit());
            }

            int frameIndex = frames.size() - 1;
            Frame frame = frames.get(frameIndex);
            Function func = module.function(frame.func).orElse(null);
            if (func == null) {
                return trap("invalid function id " + frame.func.index());
            }

            if (frame.pc >= func.code().size()) {
                // Fall off the end => implicit return unit.
                try {
                    StepResult maybe = doReturnSingle(new Value.Unit());
                    if (maybe != null) {
                        return maybe;
                    }
                    continue;
                } catch (VmTrap t) {
                    return trap(t.getMessage());
                }
            }

            Instruction inst = func.code().get(frame.pc);
            frame.pc += 1;

            if (fuel != null) {
                remaining -= 1;
            }

            try {
                StepResult out = evalInstruction(inst, frameIndex);
                if (out != null) {
                    return out;
                }
            } catch (VmTrap t) {
                return trap(t.getMessage());
            }
        }
    }

    public void resume(ContinuationHandle k, AbiValue value) throws VmError {
        Objects.requireNonNull(k, "k");
        Objects.requireNonNull(value, "value");

        if (inHostCall) {
            throw new VmError.InvalidState("vm re-entered during host call");
        }

        if (!(state instanceof VmState.Suspended s)) {
            throw new VmError.InvalidState("vm is not suspended");
        }
        if (!s.k.equals(k)) {
            throw new VmError.InvalidContinuation("continuation handle mismatch");
        }

        Integer dst = s.performDst;
        state = new VmState.Running();
        continuationGeneration = continuationGeneration + 1;

        if (dst != null) {
            if (frames.isEmpty()) {
                throw new VmError.InvalidState("resume with empty stack");
            }
            Frame top = frames.get(frames.size() - 1);
            Value v;
            try {
                v = valueFromAbi(value);
            } catch (VmTrap e) {
                throw new VmError.InvalidState("resume value conversion failed: " + e.getMessage());
            }
            try {
                writeValue(top, dst, v);
            } catch (VmTrap e) {
                throw new VmError.InvalidState("resume dst write failed: " + e.getMessage());
            }
        }
    }

    public void dropContinuation(ContinuationHandle k) throws VmError {
        Objects.requireNonNull(k, "k");

        if (inHostCall) {
            throw new VmError.InvalidState("vm re-entered during host call");
        }

        if (!(state instanceof VmState.Suspended s)) {
            throw new VmError.InvalidState("vm is not suspended");
        }
        if (!s.k.equals(k)) {
            throw new VmError.InvalidContinuation("continuation handle mismatch");
        }

        continuationGeneration = continuationGeneration + 1;
        frames.clear();
        handlers.clear();
        state = new VmState.Trapped("cancelled");
    }

    public void dropPinnedContinuation(ContinuationHandle k) throws VmError {
        Objects.requireNonNull(k, "k");

        if (inHostCall) {
            throw new VmError.InvalidState("vm re-entered during host call");
        }

        pinnedContinuations.dropPinned(k);
    }

    // ====== VM state ======

    private sealed interface VmState permits VmState.Running, VmState.Suspended, VmState.Done, VmState.Trapped {
        record Running() implements VmState {}

        record Suspended(ContinuationHandle k, Integer performDst) implements VmState {
            public Suspended {
                Objects.requireNonNull(k, "k");
            }
        }

        record Done(AbiValue value) implements VmState {
            public Done {
                Objects.requireNonNull(value, "value");
            }
        }

        record Trapped(String message) implements VmState {
            public Trapped {
                Objects.requireNonNull(message, "message");
            }
        }
    }

    // ====== Trap plumbing ======

    private static final class VmTrap extends Exception {
        VmTrap(String message) {
            super(message);
        }
    }

    private StepResult trap(String message) {
        state = new VmState.Trapped(message);
        return new StepResult.Trap(message);
    }

    // ====== ABI conversions ======

    private AbiValue valueToAbi(Value v) throws VmTrap {
        if (v instanceof Value.Unit) {
            return new AbiValue.Unit();
        }
        if (v instanceof Value.Bool b) {
            return new AbiValue.Bool(b.value());
        }
        if (v instanceof Value.Int n) {
            return new AbiValue.Int(n.value());
        }
        if (v instanceof Value.Float x) {
            return new AbiValue.Float(x.value());
        }
        if (v instanceof Value.Str s) {
            return new AbiValue.Str(s.value().asJavaString());
        }
        if (v instanceof Value.Bytes b) {
            return new AbiValue.Bytes(b.value().toByteArray());
        }
        if (v instanceof Value.Continuation c) {
            try {
                return new AbiValue.Continuation(pinnedContinuations.pin(c.token()));
            } catch (IllegalStateException e) {
                throw new VmTrap(e.getMessage());
            }
        }
        throw new VmTrap("non-ABI-safe value (" + v.kind() + ")");
    }

    private Value valueFromAbi(AbiValue v) throws VmTrap {
        if (v instanceof AbiValue.Unit) {
            return new Value.Unit();
        }
        if (v instanceof AbiValue.Bool b) {
            return new Value.Bool(b.value());
        }
        if (v instanceof AbiValue.Int n) {
            return new Value.Int(n.value());
        }
        if (v instanceof AbiValue.Float x) {
            return new Value.Float(x.value());
        }
        if (v instanceof AbiValue.Str s) {
            return new Value.Str(new RuskString(s.value()));
        }
        if (v instanceof AbiValue.Bytes b) {
            return new Value.Bytes(new RuskBytes(b.value()));
        }
        if (v instanceof AbiValue.Continuation c) {
            ContinuationToken token;
            try {
                token = pinnedContinuations.resolve(c.value());
            } catch (VmError.InvalidContinuation e) {
                throw new VmTrap(e.getMessage());
            }
            return new Value.Continuation(token);
        }
        throw new VmTrap("unknown AbiValue: " + v);
    }

    // ====== Instruction execution ======

    /**
     * @return {@code null} 表示继续执行下一条；否则表示要返回给宿主的 StepResult。
     */
    private StepResult evalInstruction(Instruction inst, int frameIndex) throws VmTrap {
        Frame frame = frames.get(frameIndex);

        if (inst instanceof Instruction.Const i) {
            writeValue(frame, i.dst(), valueFromConst(i.value()));
            return null;
        }
        if (inst instanceof Instruction.Copy i) {
            Value v = readValue(frame, i.src());
            writeValue(frame, i.dst(), v);
            return null;
        }
        if (inst instanceof Instruction.Move i) {
            Value v = readValue(frame, i.src());
            // move leaves src uninitialized
            setUninit(frame, i.src());
            writeValue(frame, i.dst(), v);
            return null;
        }
        if (inst instanceof Instruction.AsReadonly i) {
            Value v = readValue(frame, i.src());
            writeValue(frame, i.dst(), v.intoReadonlyView());
            return null;
        }
        if (inst instanceof Instruction.IsType i) {
            Value v = readValue(frame, i.value());
            TypeReps.TypeRepId target = readTypeRep(frame, i.ty());
            boolean ok = typeTest(v, target);
            writeValue(frame, i.dst(), new Value.Bool(ok));
            return null;
        }

        if (inst instanceof Instruction.MakeTypeRep i) {
            ArrayList<TypeReps.TypeRepId> argIds = new ArrayList<>(i.args().size());
            for (int r : i.args()) {
                argIds.add(readTypeRep(frame, r));
            }
            TypeReps.TypeCtor ctor;
            try {
                ctor = TypeReps.ctorFromLit(module, i.base());
            } catch (IllegalArgumentException e) {
                throw new VmTrap("make_typerep base: " + e.getMessage());
            }
            TypeReps.TypeRepId id =
                    typeReps.intern(new TypeReps.TypeRepNode(ctor, argIds));
            writeValue(frame, i.dst(), new Value.TypeRep(id));
            return null;
        }

        if (inst instanceof Instruction.MakeStruct i) {
            ArrayList<TypeReps.TypeRepId> typeArgs = new ArrayList<>(i.typeArgs().size());
            for (int r : i.typeArgs()) {
                typeArgs.add(readTypeRep(frame, r));
            }

            List<String> layout = module.structLayout(i.typeId()).orElse(null);
            if (layout == null) {
                String typeName = module.typeName(i.typeId()).orElse("<unknown>");
                throw new VmTrap("missing struct layout for `" + typeName + "`");
            }

            Value[] out = new Value[layout.size()];
            for (Instruction.MakeStruct.FieldInit f : i.fields()) {
                Value fieldValue = readValue(frame, f.reg());
                int idx = layout.indexOf(f.name());
                if (idx < 0) {
                    throw new VmTrap("missing field: " + f.name());
                }
                if (out[idx] != null) {
                    String typeName = module.typeName(i.typeId()).orElse("<unknown>");
                    throw new VmTrap(
                            "duplicate field `" + f.name() + "` in `" + typeName + "` literal");
                }
                out[idx] = fieldValue;
            }
            ArrayList<Value> fields = new ArrayList<>(out.length);
            for (int idx = 0; idx < out.length; idx++) {
                if (out[idx] == null) {
                    throw new VmTrap("missing field: " + layout.get(idx));
                }
                fields.add(out[idx]);
            }
            writeValue(frame, i.dst(), allocRef(new StructObj(i.typeId(), typeArgs, fields)));
            return null;
        }

        if (inst instanceof Instruction.MakeArray i) {
            ArrayList<Value> items = new ArrayList<>(i.items().size());
            for (int r : i.items()) {
                items.add(readValue(frame, r));
            }
            writeValue(frame, i.dst(), allocRef(new ArrayObj(items)));
            return null;
        }

        if (inst instanceof Instruction.MakeTuple i) {
            if (i.items().isEmpty()) {
                writeValue(frame, i.dst(), new Value.Unit());
                return null;
            }
            ArrayList<Value> items = new ArrayList<>(i.items().size());
            for (int r : i.items()) {
                items.add(readValue(frame, r));
            }
            writeValue(frame, i.dst(), allocRef(new TupleObj(items)));
            return null;
        }

        if (inst instanceof Instruction.MakeEnum i) {
            ArrayList<TypeReps.TypeRepId> typeArgs = new ArrayList<>(i.typeArgs().size());
            for (int r : i.typeArgs()) {
                typeArgs.add(readTypeRep(frame, r));
            }
            ArrayList<Value> fields = new ArrayList<>(i.fields().size());
            for (int r : i.fields()) {
                fields.add(readValue(frame, r));
            }
            writeValue(frame, i.dst(), allocRef(new EnumObj(i.enumTypeId(), typeArgs, i.variant(), fields)));
            return null;
        }

        if (inst instanceof Instruction.GetField i) {
            Value objV = readValue(frame, i.obj());
            if (!(objV instanceof Value.Ref ref)) {
                throw new VmTrap(
                        "type error in get_field: expected ref(struct/tuple), got " + objV.kind());
            }
            Value.RefValue rv = ref.value();
            Value out;
            if (rv.obj() instanceof StructObj s) {
                int idx = structFieldIndex(s.typeId, i.field());
                if (idx < 0 || idx >= s.fields.size()) {
                    throw new VmTrap("missing field: " + i.field());
                }
                out = s.fields.get(idx);
            } else if (rv.obj() instanceof TupleObj t) {
                Integer idx = tupleFieldIndex(i.field());
                if (idx == null || idx < 0 || idx >= t.items.size()) {
                    throw new VmTrap("missing field: " + i.field());
                }
                out = t.items.get(idx);
            } else {
                throw new VmTrap("type error in get_field: expected struct/tuple, got ref");
            }
            if (rv.readonly()) {
                out = out.intoReadonlyView();
            }
            writeValue(frame, i.dst(), out);
            return null;
        }

        if (inst instanceof Instruction.SetField i) {
            Value objV = readValue(frame, i.obj());
            if (!(objV instanceof Value.Ref ref)) {
                throw new VmTrap(
                        "type error in set_field: expected ref(struct/tuple), got " + objV.kind());
            }
            Value.RefValue rv = ref.value();
            if (rv.readonly()) {
                throw new VmTrap("illegal write through readonly reference");
            }
            Value val = readValue(frame, i.value());
            if (rv.obj() instanceof StructObj s) {
                int idx = structFieldIndex(s.typeId, i.field());
                if (idx < 0 || idx >= s.fields.size()) {
                    throw new VmTrap("missing field: " + i.field());
                }
                s.fields.set(idx, val);
            } else if (rv.obj() instanceof TupleObj t) {
                Integer idx = tupleFieldIndex(i.field());
                if (idx == null || idx < 0 || idx >= t.items.size()) {
                    throw new VmTrap("missing field: " + i.field());
                }
                t.items.set(idx, val);
            } else {
                throw new VmTrap("type error in set_field: expected struct/tuple, got ref");
            }
            return null;
        }

        if (inst instanceof Instruction.StructGet i) {
            Value objV = readValue(frame, i.obj());
            if (!(objV instanceof Value.Ref ref)) {
                throw new VmTrap(
                        "type error in struct_get: expected ref(struct), got " + objV.kind());
            }
            Value.RefValue rv = ref.value();
            if (!(rv.obj() instanceof StructObj s)) {
                throw new VmTrap("type error in struct_get: expected struct, got ref");
            }
            Value out;
            if (i.idx() < 0 || i.idx() >= s.fields.size()) {
                String fieldName =
                        module.structLayout(s.typeId)
                                .filter(names -> i.idx() < names.size())
                                .map(names -> names.get(i.idx()))
                                .orElse("#" + i.idx());
                throw new VmTrap("missing field: " + fieldName);
            }
            out = s.fields.get(i.idx());
            if (rv.readonly()) {
                out = out.intoReadonlyView();
            }
            writeValue(frame, i.dst(), out);
            return null;
        }

        if (inst instanceof Instruction.StructSet i) {
            Value objV = readValue(frame, i.obj());
            if (!(objV instanceof Value.Ref ref)) {
                throw new VmTrap(
                        "type error in struct_set: expected ref(struct), got " + objV.kind());
            }
            Value.RefValue rv = ref.value();
            if (rv.readonly()) {
                throw new VmTrap("illegal write through readonly reference");
            }
            Value val = readValue(frame, i.value());
            if (!(rv.obj() instanceof StructObj s)) {
                throw new VmTrap("type error in struct_set: expected struct, got ref");
            }
            if (i.idx() < 0 || i.idx() >= s.fields.size()) {
                throw new VmTrap("missing field: #" + i.idx());
            }
            s.fields.set(i.idx(), val);
            return null;
        }

        if (inst instanceof Instruction.TupleGet i) {
            Value tupV = readValue(frame, i.tup());
            if (!(tupV instanceof Value.Ref ref)) {
                throw new VmTrap(
                        "type error in tuple_get: expected ref(tuple), got " + tupV.kind());
            }
            Value.RefValue rv = ref.value();
            if (!(rv.obj() instanceof TupleObj t)) {
                throw new VmTrap("type error in tuple_get: expected tuple, got ref");
            }
            if (i.idx() < 0 || i.idx() >= t.items.size()) {
                throw new VmTrap("missing field: ." + i.idx());
            }
            Value out = t.items.get(i.idx());
            if (rv.readonly()) {
                out = out.intoReadonlyView();
            }
            writeValue(frame, i.dst(), out);
            return null;
        }

        if (inst instanceof Instruction.TupleSet i) {
            Value tupV = readValue(frame, i.tup());
            if (!(tupV instanceof Value.Ref ref)) {
                throw new VmTrap(
                        "type error in tuple_set: expected ref(tuple), got " + tupV.kind());
            }
            Value.RefValue rv = ref.value();
            if (rv.readonly()) {
                throw new VmTrap("illegal write through readonly reference");
            }
            Value val = readValue(frame, i.value());
            if (!(rv.obj() instanceof TupleObj t)) {
                throw new VmTrap("type error in tuple_set: expected tuple, got ref");
            }
            if (i.idx() < 0 || i.idx() >= t.items.size()) {
                throw new VmTrap("missing field: ." + i.idx());
            }
            t.items.set(i.idx(), val);
            return null;
        }

        if (inst instanceof Instruction.IndexGet i) {
            Value arrV = readValue(frame, i.arr());
            if (!(arrV instanceof Value.Ref ref)) {
                throw new VmTrap("type error in index_get: expected ref(array), got " + arrV.kind());
            }
            Value.RefValue rv = ref.value();
            if (!(rv.obj() instanceof ArrayObj arr)) {
                throw new VmTrap("type error in index_get: expected array, got ref");
            }
            long idx = readInt(frame, i.idx());
            int len = arr.items.size();
            int idxInt;
            if (idx < 0 || idx > Integer.MAX_VALUE) {
                throw new VmTrap("index out of bounds: index=" + idx + ", len=" + len);
            }
            idxInt = (int) idx;
            if (idxInt >= len) {
                throw new VmTrap("index out of bounds: index=" + idx + ", len=" + len);
            }
            Value out = arr.items.get(idxInt);
            if (rv.readonly()) {
                out = out.intoReadonlyView();
            }
            writeValue(frame, i.dst(), out);
            return null;
        }

        if (inst instanceof Instruction.IndexSet i) {
            Value arrV = readValue(frame, i.arr());
            if (!(arrV instanceof Value.Ref ref)) {
                throw new VmTrap("type error in index_set: expected ref(array), got " + arrV.kind());
            }
            Value.RefValue rv = ref.value();
            if (rv.readonly()) {
                throw new VmTrap("illegal write through readonly reference");
            }
            if (!(rv.obj() instanceof ArrayObj arr)) {
                throw new VmTrap("type error in index_set: expected array, got ref");
            }
            long idx = readInt(frame, i.idx());
            Value val = readValue(frame, i.value());
            int len = arr.items.size();
            if (idx < 0 || idx > Integer.MAX_VALUE) {
                throw new VmTrap("index out of bounds: index=" + idx + ", len=" + len);
            }
            int idxInt = (int) idx;
            if (idxInt >= len) {
                throw new VmTrap("index out of bounds: index=" + idx + ", len=" + len);
            }
            arr.items.set(idxInt, val);
            return null;
        }

        if (inst instanceof Instruction.Len i) {
            Value arrV = readValue(frame, i.arr());
            if (!(arrV instanceof Value.Ref ref)) {
                throw new VmTrap("type error in len: expected ref(array), got " + arrV.kind());
            }
            Value.RefValue rv = ref.value();
            if (!(rv.obj() instanceof ArrayObj arr)) {
                throw new VmTrap("type error in len: expected array, got ref");
            }
            writeValue(frame, i.dst(), new Value.Int(arr.items.size()));
            return null;
        }

        if (inst instanceof Instruction.IntAdd i) {
            long a = readInt(frame, i.a());
            long b = readInt(frame, i.b());
            writeValue(frame, i.dst(), new Value.Int(a + b));
            return null;
        }
        if (inst instanceof Instruction.IntSub i) {
            long a = readInt(frame, i.a());
            long b = readInt(frame, i.b());
            writeValue(frame, i.dst(), new Value.Int(a - b));
            return null;
        }
        if (inst instanceof Instruction.IntMul i) {
            long a = readInt(frame, i.a());
            long b = readInt(frame, i.b());
            writeValue(frame, i.dst(), new Value.Int(a * b));
            return null;
        }
        if (inst instanceof Instruction.IntDiv i) {
            long a = readInt(frame, i.a());
            long b = readInt(frame, i.b());
            if (b == 0) {
                throw new VmTrap("int_div: division by zero");
            }
            writeValue(frame, i.dst(), new Value.Int(a / b));
            return null;
        }
        if (inst instanceof Instruction.IntMod i) {
            long a = readInt(frame, i.a());
            long b = readInt(frame, i.b());
            if (b == 0) {
                throw new VmTrap("int_mod: modulo by zero");
            }
            writeValue(frame, i.dst(), new Value.Int(a % b));
            return null;
        }
        if (inst instanceof Instruction.IntAnd i) {
            writeValue(frame, i.dst(), new Value.Int(readInt(frame, i.a()) & readInt(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.IntOr i) {
            writeValue(frame, i.dst(), new Value.Int(readInt(frame, i.a()) | readInt(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.IntXor i) {
            writeValue(frame, i.dst(), new Value.Int(readInt(frame, i.a()) ^ readInt(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.IntShl i) {
            long a = readInt(frame, i.a());
            int sh = readShiftAmount(frame, i.b(), 64);
            writeValue(frame, i.dst(), new Value.Int(a << sh));
            return null;
        }
        if (inst instanceof Instruction.IntShr i) {
            long a = readInt(frame, i.a());
            int sh = readShiftAmount(frame, i.b(), 64);
            writeValue(frame, i.dst(), new Value.Int(a >> sh));
            return null;
        }
        if (inst instanceof Instruction.IntUShr i) {
            long a = readInt(frame, i.a());
            int sh = readShiftAmount(frame, i.b(), 64);
            writeValue(frame, i.dst(), new Value.Int(a >>> sh));
            return null;
        }
        if (inst instanceof Instruction.IntNot i) {
            writeValue(frame, i.dst(), new Value.Int(~readInt(frame, i.v())));
            return null;
        }
        if (inst instanceof Instruction.IntLt i) {
            writeValue(frame, i.dst(), new Value.Bool(readInt(frame, i.a()) < readInt(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.IntLe i) {
            writeValue(frame, i.dst(), new Value.Bool(readInt(frame, i.a()) <= readInt(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.IntGt i) {
            writeValue(frame, i.dst(), new Value.Bool(readInt(frame, i.a()) > readInt(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.IntGe i) {
            writeValue(frame, i.dst(), new Value.Bool(readInt(frame, i.a()) >= readInt(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.IntEq i) {
            writeValue(frame, i.dst(), new Value.Bool(readInt(frame, i.a()) == readInt(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.IntNe i) {
            writeValue(frame, i.dst(), new Value.Bool(readInt(frame, i.a()) != readInt(frame, i.b())));
            return null;
        }

        if (inst instanceof Instruction.ByteAnd i) {
            writeValue(frame, i.dst(), new Value.Byte(readByte(frame, i.a()) & readByte(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.ByteOr i) {
            writeValue(frame, i.dst(), new Value.Byte(readByte(frame, i.a()) | readByte(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.ByteXor i) {
            writeValue(frame, i.dst(), new Value.Byte(readByte(frame, i.a()) ^ readByte(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.ByteShl i) {
            int a = readByte(frame, i.a());
            int sh = readShiftAmount(frame, i.b(), 8);
            writeValue(frame, i.dst(), new Value.Byte((a << sh) & 0xFF));
            return null;
        }
        if (inst instanceof Instruction.ByteShr i) {
            int a = readByte(frame, i.a());
            int sh = readShiftAmount(frame, i.b(), 8);
            writeValue(frame, i.dst(), new Value.Byte((a >>> sh) & 0xFF));
            return null;
        }
        if (inst instanceof Instruction.ByteUShr i) {
            int a = readByte(frame, i.a());
            int sh = readShiftAmount(frame, i.b(), 8);
            writeValue(frame, i.dst(), new Value.Byte((a >>> sh) & 0xFF));
            return null;
        }
        if (inst instanceof Instruction.ByteNot i) {
            writeValue(frame, i.dst(), new Value.Byte((~readByte(frame, i.v())) & 0xFF));
            return null;
        }

        if (inst instanceof Instruction.BoolNot i) {
            boolean v = readBool(frame, i.v());
            writeValue(frame, i.dst(), new Value.Bool(!v));
            return null;
        }
        if (inst instanceof Instruction.BoolEq i) {
            writeValue(frame, i.dst(), new Value.Bool(readBool(frame, i.a()) == readBool(frame, i.b())));
            return null;
        }
        if (inst instanceof Instruction.BoolNe i) {
            writeValue(frame, i.dst(), new Value.Bool(readBool(frame, i.a()) != readBool(frame, i.b())));
            return null;
        }

        if (inst instanceof Instruction.Call i) {
            return doCall(i.dst(), i.func(), i.args(), frameIndex);
        }
        if (inst instanceof Instruction.CallMulti i) {
            return doCallMulti(i.dsts(), i.func(), i.args(), frameIndex);
        }
        if (inst instanceof Instruction.ICall i) {
            Value fnptr = readValue(frame, i.fnptr());
            if (!(fnptr instanceof Value.Function f)) {
                throw new VmTrap("type error in icall: expected function, got " + fnptr.kind());
            }
            return doCall(i.dst(), new CallTarget.Bc(f.value()), i.args(), frameIndex);
        }
        if (inst instanceof Instruction.VCall i) {
            return doVCall(i.dst(), i.obj(), i.method(), i.methodTypeArgs(), i.args(), frameIndex);
        }

        if (inst instanceof Instruction.PushHandler i) {
            ArrayList<RuntimeHandlerClause> runtimeClauses = new ArrayList<>(i.clauses().size());
            for (HandlerClause clause : i.clauses()) {
                ArrayList<TypeReps.TypeRepId> ifaceArgs = new ArrayList<>(clause.effect().interfaceArgs().size());
                for (int r : clause.effect().interfaceArgs()) {
                    ifaceArgs.add(readTypeRep(frame, r));
                }
                runtimeClauses.add(
                        new RuntimeHandlerClause(
                                new RuntimeEffectId(
                                        clause.effect().interfaceName(), ifaceArgs, clause.effect().method()),
                                clause.argPatterns(),
                                clause.targetPc(),
                                clause.paramRegs()));
            }
            handlers.add(new HandlerEntry(frameIndex, runtimeClauses));
            return null;
        }

        if (inst instanceof Instruction.PopHandler) {
            if (handlers.isEmpty()) {
                throw new VmTrap("mismatched pop_handler");
            }
            HandlerEntry top = handlers.get(handlers.size() - 1);
            if (top.ownerDepth != frameIndex) {
                throw new VmTrap("mismatched pop_handler");
            }
            handlers.remove(handlers.size() - 1);
            return null;
        }

        if (inst instanceof Instruction.Perform i) {
            return doPerform(i.dst(), i.effect(), i.args(), frameIndex);
        }

        if (inst instanceof Instruction.Resume i) {
            return doResumeFromToken(i.dst(), i.k(), i.value(), frameIndex, false);
        }
        if (inst instanceof Instruction.ResumeTail i) {
            return doResumeFromToken(null, i.k(), i.value(), frameIndex, true);
        }

        if (inst instanceof Instruction.Jump i) {
            frame.pc = i.targetPc();
            return null;
        }
        if (inst instanceof Instruction.JumpIf i) {
            boolean cond = readBool(frame, i.cond());
            frame.pc = cond ? i.thenPc() : i.elsePc();
            return null;
        }
        if (inst instanceof Instruction.Switch i) {
            Value scrutinee = readValue(frame, i.value());
            boolean matched = false;
            for (SwitchCase c : i.cases()) {
                ArrayList<Value> binds = new ArrayList<>();
                if (matchPattern(c.pattern(), scrutinee, binds)) {
                    if (binds.size() != c.paramRegs().size()) {
                        throw new VmTrap("switch bind count mismatch");
                    }
                    for (int idx = 0; idx < binds.size(); idx++) {
                        writeValue(frame, c.paramRegs().get(idx), binds.get(idx));
                    }
                    frame.pc = c.targetPc();
                    matched = true;
                    break;
                }
            }
            if (!matched) {
                frame.pc = i.defaultPc();
            }
            return null;
        }

        if (inst instanceof Instruction.Return i) {
            Value v = readValue(frame, i.value());
            StepResult maybe = doReturnSingle(v);
            if (maybe != null) {
                return maybe;
            }
            return null;
        }
        if (inst instanceof Instruction.ReturnMulti i) {
            ArrayList<Value> values = new ArrayList<>(i.values().size());
            for (int r : i.values()) {
                values.add(readValue(frame, r));
            }
            StepResult maybe = doReturnMulti(values);
            if (maybe != null) {
                return maybe;
            }
            return null;
        }
        if (inst instanceof Instruction.Trap i) {
            return trap(i.message());
        }

        throw new VmTrap("unknown instruction");
    }

    private Value valueFromConst(ConstValue v) throws VmTrap {
        if (v instanceof ConstValue.Unit) {
            return new Value.Unit();
        }
        if (v instanceof ConstValue.Bool b) {
            return new Value.Bool(b.value());
        }
        if (v instanceof ConstValue.Int n) {
            return new Value.Int(n.value());
        }
        if (v instanceof ConstValue.Float x) {
            return new Value.Float(x.value());
        }
        if (v instanceof ConstValue.Str s) {
            return new Value.Str(new RuskString(s.value()));
        }
        if (v instanceof ConstValue.Bytes b) {
            return new Value.Bytes(new RuskBytes(b.value()));
        }
        if (v instanceof ConstValue.TypeRep t) {
            TypeReps.TypeCtor ctor;
            try {
                ctor = TypeReps.ctorFromLit(module, t.value());
            } catch (IllegalArgumentException e) {
                throw new VmTrap("const typerep: " + e.getMessage());
            }
            TypeReps.TypeRepId id = typeReps.intern(new TypeReps.TypeRepNode(ctor, List.of()));
            return new Value.TypeRep(id);
        }
        if (v instanceof ConstValue.Function f) {
            return new Value.Function(f.value());
        }
        throw new VmTrap("unknown const value");
    }

    private Value allocRef(HeapObject obj) {
        return new Value.Ref(new Value.RefValue(obj, false));
    }

    private void setUninit(Frame frame, int reg) throws VmTrap {
        if (reg < 0 || reg >= frame.regs.length) {
            // Match Rust behavior: out-of-range read/write is effectively invalid; keep message simple.
            throw new VmTrap("write to out-of-range reg " + reg);
        }
        frame.regs[reg] = null;
    }

    private Value readValue(Frame frame, int reg) throws VmTrap {
        if (reg < 0 || reg >= frame.regs.length) {
            throw new VmTrap("read from uninitialized reg " + reg);
        }
        Value v = frame.regs[reg];
        if (v == null) {
            throw new VmTrap("read from uninitialized reg " + reg);
        }
        return v;
    }

    private void writeValue(Frame frame, int reg, Value value) throws VmTrap {
        if (reg < 0 || reg >= frame.regs.length) {
            throw new VmTrap("write to out-of-range reg " + reg);
        }
        frame.regs[reg] = value;
    }

    private long readInt(Frame frame, int reg) throws VmTrap {
        Value v = readValue(frame, reg);
        if (v instanceof Value.Int n) {
            return n.value();
        }
        throw new VmTrap("expected int, got " + v.kind());
    }

    private int readByte(Frame frame, int reg) throws VmTrap {
        Value v = readValue(frame, reg);
        if (v instanceof Value.Byte b) {
            return b.value();
        }
        throw new VmTrap("expected byte, got " + v.kind());
    }

    private int readShiftAmount(Frame frame, int reg, int width) throws VmTrap {
        long shift = readInt(frame, reg);
        if (shift < 0 || shift >= width) {
            throw new VmTrap("shift amount out of range: " + shift);
        }
        return (int) shift;
    }

    private boolean readBool(Frame frame, int reg) throws VmTrap {
        Value v = readValue(frame, reg);
        if (v instanceof Value.Bool b) {
            return b.value();
        }
        throw new VmTrap("expected bool, got " + v.kind());
    }

    private TypeReps.TypeRepId readTypeRep(Frame frame, int reg) throws VmTrap {
        Value v = readValue(frame, reg);
        if (v instanceof Value.TypeRep t) {
            return t.value();
        }
        throw new VmTrap("type error in typerep: expected typerep, got " + v.kind());
    }

    private Integer tupleFieldIndex(String field) {
        if (!field.startsWith(".")) {
            return null;
        }
        try {
            return Integer.parseInt(field.substring(1));
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private int structFieldIndex(TypeId typeId, String field) throws VmTrap {
        List<String> layout = module.structLayout(typeId).orElse(null);
        if (layout == null) {
            String typeName = module.typeName(typeId).orElse("<unknown>");
            throw new VmTrap("missing struct layout for `" + typeName + "`");
        }
        int idx = layout.indexOf(field);
        if (idx < 0) {
            throw new VmTrap("missing field: " + field);
        }
        return idx;
    }

    private boolean typeTest(Value value, TypeReps.TypeRepId target) throws VmTrap {
        TypeReps.TypeRepNode targetNode = typeReps.node(target);
        if (targetNode == null) {
            throw new VmTrap("invalid typerep(" + target.index() + ")");
        }

        TypeReps.TypeCtor ctor = targetNode.ctor();
        if (ctor instanceof TypeReps.TypeCtor.Unit) {
            return value instanceof Value.Unit;
        }
        if (ctor instanceof TypeReps.TypeCtor.Never) {
            return false;
        }
        if (ctor instanceof TypeReps.TypeCtor.Bool) {
            return value instanceof Value.Bool;
        }
        if (ctor instanceof TypeReps.TypeCtor.Int) {
            return value instanceof Value.Int;
        }
        if (ctor instanceof TypeReps.TypeCtor.Float) {
            return value instanceof Value.Float;
        }
        if (ctor instanceof TypeReps.TypeCtor.Byte) {
            return value instanceof Value.Byte;
        }
        if (ctor instanceof TypeReps.TypeCtor.Char) {
            return value instanceof Value.Char;
        }
        if (ctor instanceof TypeReps.TypeCtor.String) {
            return value instanceof Value.Str;
        }
        if (ctor instanceof TypeReps.TypeCtor.Bytes) {
            return value instanceof Value.Bytes;
        }
        if (ctor instanceof TypeReps.TypeCtor.Array) {
            if (value instanceof Value.Ref r) {
                return r.value().obj() instanceof ArrayObj;
            }
            return false;
        }
        if (ctor instanceof TypeReps.TypeCtor.Tuple t) {
            int arity = t.arity();
            if (value instanceof Value.Unit) {
                return arity == 0;
            }
            if (value instanceof Value.Ref r) {
                if (r.value().obj() instanceof TupleObj tup) {
                    return tup.items.size() == arity;
                }
                return false;
            }
            return false;
        }
        if (ctor instanceof TypeReps.TypeCtor.Struct s) {
            if (value instanceof Value.Ref r) {
                if (r.value().obj() instanceof StructObj obj) {
                    return obj.typeId.equals(s.typeId()) && obj.typeArgs.equals(targetNode.args());
                }
                return false;
            }
            return false;
        }
        if (ctor instanceof TypeReps.TypeCtor.Enum e) {
            if (value instanceof Value.Ref r) {
                if (r.value().obj() instanceof EnumObj obj) {
                    return obj.typeId.equals(e.typeId()) && obj.typeArgs.equals(targetNode.args());
                }
                return false;
            }
            return false;
        }
        if (ctor instanceof TypeReps.TypeCtor.Interface iface) {
            TypeId dynTypeId;
            List<TypeReps.TypeRepId> dynTypeArgs;
            if (value instanceof Value.Unit) {
                dynTypeId = primitiveTypeIds.unit();
                dynTypeArgs = List.of();
            } else if (value instanceof Value.Bool) {
                dynTypeId = primitiveTypeIds.bool();
                dynTypeArgs = List.of();
            } else if (value instanceof Value.Int) {
                dynTypeId = primitiveTypeIds.intTy();
                dynTypeArgs = List.of();
            } else if (value instanceof Value.Float) {
                dynTypeId = primitiveTypeIds.floatTy();
                dynTypeArgs = List.of();
            } else if (value instanceof Value.Byte) {
                dynTypeId = primitiveTypeIds.byteTy();
                dynTypeArgs = List.of();
            } else if (value instanceof Value.Char) {
                dynTypeId = primitiveTypeIds.charTy();
                dynTypeArgs = List.of();
            } else if (value instanceof Value.Str) {
                dynTypeId = primitiveTypeIds.stringTy();
                dynTypeArgs = List.of();
            } else if (value instanceof Value.Bytes) {
                dynTypeId = primitiveTypeIds.bytesTy();
                dynTypeArgs = List.of();
            } else if (value instanceof Value.Ref r) {
                HeapObject obj = r.value().obj();
                if (obj instanceof StructObj s) {
                    dynTypeId = s.typeId;
                    dynTypeArgs = s.typeArgs;
                } else if (obj instanceof EnumObj e) {
                    dynTypeId = e.typeId;
                    dynTypeArgs = e.typeArgs;
                } else {
                    return false;
                }
            } else {
                return false;
            }

            List<TypeId> ifaces =
                    module.interfaceImplsOf(dynTypeId)
                            .orElseThrow(
                                    () -> new VmTrap("invalid TypeId " + dynTypeId.index() + " in interface_impls lookup"));
            boolean implementsIface = ifaces.contains(iface.typeId());
            if (!implementsIface) {
                return false;
            }

            if (targetNode.args().isEmpty()) {
                return true;
            }
            if (dynTypeArgs.size() < targetNode.args().size()) {
                return false;
            }
            return dynTypeArgs.subList(0, targetNode.args().size()).equals(targetNode.args());
        }
        if (ctor instanceof TypeReps.TypeCtor.Fn || ctor instanceof TypeReps.TypeCtor.Cont) {
            return false;
        }
        throw new VmTrap("unknown typerep ctor");
    }

    // ====== Calls / returns ======

    private StepResult doCall(Integer dst, CallTarget target, List<Integer> argRegs, int callerFrameIndex)
            throws VmTrap {
        Frame caller = frames.get(callerFrameIndex);

        if (target instanceof CallTarget.IntrinsicTarget intr) {
            Value v = evalCoreIntrinsic(intr.intrinsic(), caller, argRegs);
            if (dst != null) {
                writeValue(caller, dst, v);
            }
            return null;
        }
        if (target instanceof CallTarget.Host host) {
            callHostImport(dst, host.hostImport(), caller, argRegs);
            return null;
        }
        if (!(target instanceof CallTarget.Bc bc)) {
            throw new VmTrap("invalid call target");
        }

        FunctionId fnId = bc.function();
        Function callee = module.function(fnId).orElse(null);
        if (callee == null) {
            throw new VmTrap("invalid function id " + fnId.index());
        }

        ArrayList<Value> args = new ArrayList<>(argRegs.size());
        for (int r : argRegs) {
            args.add(readValue(caller, r));
        }
        if (args.size() != callee.paramCount()) {
            throw new VmTrap(
                    "call arity mismatch: expected " + callee.paramCount() + " args but got " + args.size());
        }

        Value[] regs = new Value[callee.regCount()];
        for (int i = 0; i < args.size(); i++) {
            regs[i] = args.get(i);
        }
        frames.add(new Frame(fnId, 0, regs, ReturnDsts.fromOption(dst)));
        return null;
    }

    private StepResult doCallMulti(
            List<Integer> dsts, CallTarget target, List<Integer> argRegs, int callerFrameIndex) throws VmTrap {
        if (!(target instanceof CallTarget.Bc bc)) {
            throw new VmTrap("CallMulti only supports bytecode functions");
        }
        Frame caller = frames.get(callerFrameIndex);

        FunctionId fnId = bc.function();
        Function callee = module.function(fnId).orElse(null);
        if (callee == null) {
            throw new VmTrap("invalid function id " + fnId.index());
        }

        ArrayList<Value> args = new ArrayList<>(argRegs.size());
        for (int r : argRegs) {
            args.add(readValue(caller, r));
        }
        if (args.size() != callee.paramCount()) {
            throw new VmTrap(
                    "call arity mismatch: expected " + callee.paramCount() + " args but got " + args.size());
        }

        Value[] regs = new Value[callee.regCount()];
        for (int i = 0; i < args.size(); i++) {
            regs[i] = args.get(i);
        }
        frames.add(new Frame(fnId, 0, regs, new ReturnDsts.Multi(dsts)));
        return null;
    }

    private StepResult doVCall(
            Integer dst,
            int objReg,
            MethodId method,
            List<Integer> methodTypeArgs,
            List<Integer> args,
            int callerFrameIndex)
            throws VmTrap {
        Frame caller = frames.get(callerFrameIndex);

        Value recv = readValue(caller, objReg);

        if (methodTypeArgs.isEmpty()) {
            Value fastOut = tryVCallFastPath(recv, method, args, caller);
            if (fastOut != null) {
                if (dst != null) {
                    writeValue(caller, dst, fastOut);
                }
                return null;
            }
        }

        TypeId typeId;
        List<TypeReps.TypeRepId> typeArgs;
        if (recv instanceof Value.Unit) {
            typeId = primitiveTypeIds.unit();
            typeArgs = List.of();
        } else if (recv instanceof Value.Bool) {
            typeId = primitiveTypeIds.bool();
            typeArgs = List.of();
        } else if (recv instanceof Value.Int) {
            typeId = primitiveTypeIds.intTy();
            typeArgs = List.of();
        } else if (recv instanceof Value.Float) {
            typeId = primitiveTypeIds.floatTy();
            typeArgs = List.of();
        } else if (recv instanceof Value.Byte) {
            typeId = primitiveTypeIds.byteTy();
            typeArgs = List.of();
        } else if (recv instanceof Value.Char) {
            typeId = primitiveTypeIds.charTy();
            typeArgs = List.of();
        } else if (recv instanceof Value.Str) {
            typeId = primitiveTypeIds.stringTy();
            typeArgs = List.of();
        } else if (recv instanceof Value.Bytes) {
            typeId = primitiveTypeIds.bytesTy();
            typeArgs = List.of();
        } else if (recv instanceof Value.Ref recvRef) {
            HeapObject obj = recvRef.value().obj();
            if (obj instanceof StructObj s) {
                typeId = s.typeId;
                typeArgs = s.typeArgs;
            } else if (obj instanceof EnumObj e) {
                typeId = e.typeId;
                typeArgs = e.typeArgs;
            } else {
                throw new VmTrap("type error in vcall: expected struct|enum, got " + recv.kind());
            }
        } else {
            throw new VmTrap(
                    "type error in vcall: expected struct|enum ref or primitive value, got " + recv.kind());
        }

        FunctionId fnId =
                module.vcallTarget(typeId, method)
                        .orElseThrow(
                                () -> {
                                    String typeName = module.typeName(typeId).orElse("<unknown>");
                                    String methodName = module.methodName(method).orElse("<unknown>");
                                    return new VmTrap("unresolved vcall method: " + methodName + " on " + typeName);
                                });
        Function callee = module.function(fnId).orElse(null);
        if (callee == null) {
            throw new VmTrap("invalid function id " + fnId.index());
        }

        ArrayList<Value> argValues =
                new ArrayList<>(typeArgs.size() + methodTypeArgs.size() + args.size() + 1);
        for (TypeReps.TypeRepId id : typeArgs) {
            argValues.add(new Value.TypeRep(id));
        }
        for (int r : methodTypeArgs) {
            argValues.add(new Value.TypeRep(readTypeRep(caller, r)));
        }
        argValues.add(recv);
        for (int r : args) {
            argValues.add(readValue(caller, r));
        }

        if (argValues.size() != callee.paramCount()) {
            throw new VmTrap(
                    "vcall arity mismatch: expected " + callee.paramCount() + " args but got " + argValues.size());
        }

        Value[] regs = new Value[callee.regCount()];
        for (int i = 0; i < argValues.size(); i++) {
            regs[i] = argValues.get(i);
        }
        frames.add(new Frame(fnId, 0, regs, ReturnDsts.fromOption(dst)));
        return null;
    }

    private Value tryVCallFastPath(Value recv, MethodId method, List<Integer> args, Frame caller) throws VmTrap {
        if (vcallFastPathIds.hash() != null && vcallFastPathIds.hash().equals(method)) {
            if (!args.isEmpty()) {
                return null;
            }
            if (recv instanceof Value.Unit) {
                return new Value.Int(0);
            }
            if (recv instanceof Value.Bool b) {
                return new Value.Int(fnv1a64(leBytes(b.value() ? 1 : 0)));
            }
            if (recv instanceof Value.Int n) {
                return new Value.Int(fnv1a64(leBytes(n.value())));
            }
            if (recv instanceof Value.Byte v) {
                return new Value.Int(fnv1a64(leBytes(v.value())));
            }
            if (recv instanceof Value.Char v) {
                return new Value.Int(fnv1a64(leBytes(v.codePoint())));
            }
            if (recv instanceof Value.Str s) {
                return new Value.Int(fnv1a64(s.value().toUtf8Bytes()));
            }
            if (recv instanceof Value.Bytes b) {
                return new Value.Int(fnv1a64(b.value().toByteArray()));
            }
            return null;
        }

        if (vcallFastPathIds.eq() != null && vcallFastPathIds.eq().equals(method)) {
            if (args.size() != 1) {
                return null;
            }
            Value other = readValue(caller, args.get(0));
            if (recv instanceof Value.Unit) {
                return (other instanceof Value.Unit) ? new Value.Bool(true) : null;
            }
            if (recv instanceof Value.Bool a) {
                return (other instanceof Value.Bool b) ? new Value.Bool(a.value() == b.value()) : null;
            }
            if (recv instanceof Value.Int a) {
                return (other instanceof Value.Int b) ? new Value.Bool(a.value() == b.value()) : null;
            }
            if (recv instanceof Value.Float a) {
                return (other instanceof Value.Float b) ? new Value.Bool(a.value() == b.value()) : null;
            }
            if (recv instanceof Value.Byte a) {
                return (other instanceof Value.Byte b) ? new Value.Bool(a.value() == b.value()) : null;
            }
            if (recv instanceof Value.Char a) {
                return (other instanceof Value.Char b) ? new Value.Bool(a.codePoint() == b.codePoint()) : null;
            }
            if (recv instanceof Value.Str a) {
                return (other instanceof Value.Str b) ? new Value.Bool(a.value().equals(b.value())) : null;
            }
            if (recv instanceof Value.Bytes a) {
                return (other instanceof Value.Bytes b) ? new Value.Bool(a.value().equals(b.value())) : null;
            }
            return null;
        }

        if (vcallFastPathIds.ne() != null && vcallFastPathIds.ne().equals(method)) {
            if (args.size() != 1) {
                return null;
            }
            Value other = readValue(caller, args.get(0));
            if (recv instanceof Value.Unit) {
                return (other instanceof Value.Unit) ? new Value.Bool(false) : null;
            }
            if (recv instanceof Value.Bool a) {
                return (other instanceof Value.Bool b) ? new Value.Bool(a.value() != b.value()) : null;
            }
            if (recv instanceof Value.Int a) {
                return (other instanceof Value.Int b) ? new Value.Bool(a.value() != b.value()) : null;
            }
            if (recv instanceof Value.Float a) {
                return (other instanceof Value.Float b) ? new Value.Bool(a.value() != b.value()) : null;
            }
            if (recv instanceof Value.Byte a) {
                return (other instanceof Value.Byte b) ? new Value.Bool(a.value() != b.value()) : null;
            }
            if (recv instanceof Value.Char a) {
                return (other instanceof Value.Char b) ? new Value.Bool(a.codePoint() != b.codePoint()) : null;
            }
            if (recv instanceof Value.Str a) {
                return (other instanceof Value.Str b) ? new Value.Bool(!a.value().equals(b.value())) : null;
            }
            if (recv instanceof Value.Bytes a) {
                return (other instanceof Value.Bytes b) ? new Value.Bool(!a.value().equals(b.value())) : null;
            }
            return null;
        }

        return null;
    }

    private StepResult doReturnSingle(Value v) throws VmTrap {
        ReturnDsts returnDsts = frames.get(frames.size() - 1).returnDsts;
        frames.remove(frames.size() - 1);
        boolean handlersPopped = unwindHandlersToStackLen(frames.size());

        if (!frames.isEmpty()) {
            Frame caller = frames.get(frames.size() - 1);
            if (returnDsts instanceof ReturnDsts.None) {
                return null;
            }
            if (returnDsts instanceof ReturnDsts.One one) {
                writeValue(caller, one.dst(), v);
                return null;
            }
            if (returnDsts instanceof ReturnDsts.Multi multi) {
                throw new VmTrap(
                        "single-value return does not match multi-return destination (dsts="
                                + multi.dsts().size()
                                + ")");
            }
            throw new VmTrap("invalid return dsts");
        }

        if (!(returnDsts instanceof ReturnDsts.None)) {
            throw new VmTrap("non-empty return destination at entry return");
        }

        AbiValue abi = valueToAbi(v);
        state = new VmState.Done(abi);
        return new StepResult.Done(abi);
    }

    private StepResult doReturnMulti(List<Value> values) throws VmTrap {
        ReturnDsts returnDsts = frames.get(frames.size() - 1).returnDsts;
        frames.remove(frames.size() - 1);
        boolean handlersPopped = unwindHandlersToStackLen(frames.size());

        if (!frames.isEmpty()) {
            Frame caller = frames.get(frames.size() - 1);
            if (!(returnDsts instanceof ReturnDsts.Multi multi)) {
                throw new VmTrap("multi-value return does not match caller destination");
            }
            if (multi.dsts().size() != values.size()) {
                throw new VmTrap(
                        "return arity mismatch: expected "
                                + multi.dsts().size()
                                + " values but got "
                                + values.size());
            }
            for (int i = 0; i < values.size(); i++) {
                writeValue(caller, multi.dsts().get(i), values.get(i));
            }
            return null;
        }

        throw new VmTrap("multi-value return from entry frame is not supported");
    }

    private boolean unwindHandlersToStackLen(int stackLen) {
        boolean popped = false;
        while (!handlers.isEmpty()) {
            HandlerEntry top = handlers.get(handlers.size() - 1);
            if (top.ownerDepth < stackLen) {
                break;
            }
            handlers.remove(handlers.size() - 1);
            popped = true;
        }
        return popped;
    }

    // ====== Host imports ======

    private void callHostImport(Integer dst, HostImportId hid, Frame frame, List<Integer> argRegs) throws VmTrap {
        int idx = hid.index();
        if (idx < 0 || idx >= module.hostImports().size()) {
            throw new VmTrap("invalid host import id " + idx);
        }
        HostImport imp = module.hostImports().get(idx);
        HostFn fn = hostFns[idx];
        if (fn == null) {
            throw new VmTrap("missing host import implementation: `" + imp.name() + "`");
        }

        if (argRegs.size() != imp.sig().params().size()) {
            throw new VmTrap(
                    "host call `"
                            + imp.name()
                            + "` arity mismatch: expected "
                            + imp.sig().params().size()
                            + " args but got "
                            + argRegs.size());
        }

        ArrayList<AbiValue> abiArgs = new ArrayList<>(argRegs.size());
        for (int i = 0; i < argRegs.size(); i++) {
            Value v = readValue(frame, argRegs.get(i));
            AbiType expected = imp.sig().params().get(i);
            AbiValue abi;
            try {
                abi = valueToAbi(v);
            } catch (VmTrap e) {
                throw new VmTrap(
                        "host call `"
                                + imp.name()
                                + "` arg type mismatch: expected "
                                + expected
                                + ", got "
                                + v.kind());
            }
            if (abi.ty() != expected) {
                throw new VmTrap(
                        "host call `"
                                + imp.name()
                                + "` arg type mismatch: expected "
                                + expected
                                + ", got "
                                + abi.ty());
            }
            abiArgs.add(abi);
        }

        inHostCall = true;
        AbiValue abiRet;
        try {
            abiRet = fn.call(abiArgs);
        } catch (Exception e) {
            inHostCall = false;
            throw new VmTrap("host call `" + imp.name() + "` failed: " + e.getMessage());
        } finally {
            inHostCall = false;
        }

        if (abiRet == null) {
            throw new VmTrap("host call `" + imp.name() + "` failed: null return");
        }
        if (abiRet.ty() != imp.sig().ret()) {
            throw new VmTrap(
                    "host call `"
                            + imp.name()
                            + "` return type mismatch: expected "
                            + imp.sig().ret()
                            + ", got "
                            + abiRet.ty());
        }

        if (dst != null) {
            writeValue(frame, dst, valueFromAbi(abiRet));
        }
    }

    // ====== Effects ======

    private StepResult doPerform(Integer dst, EffectSpec effect, List<Integer> argRegs, int frameIndex)
            throws VmTrap {
        Frame frame = frames.get(frameIndex);

        ArrayList<TypeReps.TypeRepId> interfaceArgs = new ArrayList<>(effect.interfaceArgs().size());
        for (int r : effect.interfaceArgs()) {
            interfaceArgs.add(readTypeRep(frame, r));
        }

        String effectInterface = effect.interfaceName();
        String effectMethod = effect.method();

        ArrayList<Value> argValues = new ArrayList<>(argRegs.size());
        for (int r : argRegs) {
            argValues.add(readValue(frame, r));
        }

        // Try in-VM handlers (from newest to oldest).
        HandlerMatch match = findHandlerForEffectSpec(effectInterface, interfaceArgs, effectMethod, argValues);
        if (match != null) {
            int handlerIndex = match.handlerIndex;
            int clauseIndex = match.clauseIndex;
            ArrayList<Value> binds = match.binds;

            HandlerEntry handler = handlers.get(handlerIndex);
            RuntimeHandlerClause clause = handler.clauses.get(clauseIndex);

            int handlerOwnerDepth = handler.ownerDepth;
            int clauseTargetPc = clause.targetPc;
            List<Integer> clauseParamRegs = clause.paramRegs;

            int expectedMin = binds.size();
            int expectedMax = binds.size() + 1;
            if (clauseParamRegs.size() != expectedMin && clauseParamRegs.size() != expectedMax) {
                throw new VmTrap("handler param_regs length mismatch");
            }

            if (clauseParamRegs.size() == expectedMin) {
                // Abortive handler: unwind and jump to handler clause without capturing.
                frames.subList(handlerOwnerDepth + 1, frames.size()).clear();
                handlers.subList(handlerIndex + 1, handlers.size()).clear();

                Frame handlerFrame = frames.get(handlerOwnerDepth);
                for (int i = 0; i < binds.size(); i++) {
                    writeValue(handlerFrame, clauseParamRegs.get(i), binds.get(i));
                }
                handlerFrame.pc = clauseTargetPc;
                return null;
            }

            // Capture continuation.
            Frame ownerSnapshot = frames.get(handlerOwnerDepth).deepCopy();
            List<Frame> movedFrames = new ArrayList<>(frames.subList(handlerOwnerDepth + 1, frames.size()));
            frames.subList(handlerOwnerDepth + 1, frames.size()).clear();

            ArrayList<Frame> capturedFrames = new ArrayList<>(1 + movedFrames.size());
            capturedFrames.add(ownerSnapshot);
            capturedFrames.addAll(movedFrames);

            List<HandlerEntry> movedHandlersAbove =
                    new ArrayList<>(handlers.subList(handlerIndex + 1, handlers.size()));
            handlers.subList(handlerIndex + 1, handlers.size()).clear();

            ArrayList<HandlerEntry> capturedHandlers = new ArrayList<>();
            for (HandlerEntry entry : handlers) {
                if (entry.ownerDepth < handlerOwnerDepth) {
                    continue;
                }
                capturedHandlers.add(new HandlerEntry(entry.ownerDepth - handlerOwnerDepth, entry.clauses));
            }
            for (HandlerEntry entry : movedHandlersAbove) {
                int rebased = entry.ownerDepth - handlerOwnerDepth;
                if (rebased < 0) {
                    throw new VmTrap("invalid handler owner depth");
                }
                capturedHandlers.add(new HandlerEntry(rebased, entry.clauses));
            }

            // Ensure destination is uninitialized in captured state until resume injects it.
            if (dst != null) {
                Frame top = capturedFrames.get(capturedFrames.size() - 1);
                if (dst < 0 || dst >= top.regs.length) {
                    throw new VmTrap("perform dst reg " + dst + " out of range");
                }
                top.regs[dst] = null;
            }

            ContinuationToken token =
                    new ContinuationToken(
                            new ContinuationToken.ContinuationState(capturedFrames, capturedHandlers, dst));

            Frame handlerFrame = frames.get(handlerOwnerDepth);
            for (int i = 0; i < binds.size(); i++) {
                writeValue(handlerFrame, clauseParamRegs.get(i), binds.get(i));
            }
            int kReg = clauseParamRegs.get(expectedMax - 1);
            writeValue(handlerFrame, kReg, new Value.Continuation(token));
            handlerFrame.pc = clauseTargetPc;
            return null;
        }

        // No in-VM handler => attempt externalized effect request.
        if (!interfaceArgs.isEmpty()) {
            throw new VmTrap("unhandled effect: " + effectInterface + "." + effectMethod);
        }

        EffectId effectId =
                module.externalEffectId(effectInterface, effectMethod).orElse(null);
        if (effectId == null) {
            throw new VmTrap("unhandled effect: " + effectInterface + "." + effectMethod);
        }
        ExternalEffectDecl decl =
                module.externalEffect(effectId).orElseThrow(() -> new VmTrap("invalid effect id " + effectId.index()));

        if (argValues.size() != decl.sig().params().size()) {
            throw new VmTrap(
                    "external effect `"
                            + decl.interfaceName()
                            + "."
                            + decl.method()
                            + "` arity mismatch: expected "
                            + decl.sig().params().size()
                            + " args but got "
                            + argValues.size());
        }

        ArrayList<AbiValue> abiArgs = new ArrayList<>(argValues.size());
        for (int i = 0; i < argValues.size(); i++) {
            Value v = argValues.get(i);
            AbiType expected = decl.sig().params().get(i);
            AbiValue abi;
            try {
                abi = valueToAbi(v);
            } catch (VmTrap e) {
                throw new VmTrap(
                        "external effect `"
                                + decl.interfaceName()
                                + "."
                                + decl.method()
                                + "` arg type mismatch: expected "
                                + expected
                                + ", got "
                                + v.kind());
            }
            if (abi.ty() != expected) {
                throw new VmTrap(
                        "external effect `"
                                + decl.interfaceName()
                                + "."
                                + decl.method()
                                + "` arg type mismatch: expected "
                                + expected
                                + ", got "
                                + abi.ty());
            }
            abiArgs.add(abi);
        }

        ContinuationHandle k = new ContinuationHandle(0, continuationGeneration);
        state = new VmState.Suspended(k, dst);
        return new StepResult.Request(effectId, abiArgs, k);
    }

    private record HandlerMatch(int handlerIndex, int clauseIndex, ArrayList<Value> binds) {}

    private HandlerMatch findHandlerForEffectSpec(
            String effectInterface, List<TypeReps.TypeRepId> interfaceArgs, String effectMethod, List<Value> args)
            throws VmTrap {
        for (int handlerIndex = handlers.size() - 1; handlerIndex >= 0; handlerIndex--) {
            HandlerEntry handler = handlers.get(handlerIndex);
            for (int clauseIndex = 0; clauseIndex < handler.clauses.size(); clauseIndex++) {
                RuntimeHandlerClause clause = handler.clauses.get(clauseIndex);
                if (!clause.effect.interfaceName.equals(effectInterface)
                        || !clause.effect.method.equals(effectMethod)
                        || !clause.effect.interfaceArgs.equals(interfaceArgs)) {
                    continue;
                }
                if (clause.argPatterns.size() != args.size()) {
                    continue;
                }
                ArrayList<Value> binds = new ArrayList<>();
                boolean ok = true;
                for (int i = 0; i < args.size(); i++) {
                    if (!matchPattern(clause.argPatterns.get(i), args.get(i), binds)) {
                        ok = false;
                        break;
                    }
                }
                if (ok) {
                    return new HandlerMatch(handlerIndex, clauseIndex, binds);
                }
            }
        }
        return null;
    }

    private StepResult doResumeFromToken(
            Integer dst, int kReg, int valueReg, int frameIndex, boolean tail) throws VmTrap {
        Frame frame = frames.get(frameIndex);

        Value kValue = readValue(frame, kReg);
        if (!(kValue instanceof Value.Continuation c)) {
            throw new VmTrap("invalid resume");
        }
        Value v = readValue(frame, valueReg);

        ContinuationToken.ContinuationState cont = c.token().takeState();
        if (cont == null) {
            throw new VmTrap("invalid resume");
        }

        if (cont.performDst() != null) {
            Frame top = cont.frames().get(cont.frames().size() - 1);
            writeValue(top, cont.performDst(), v);
        }

        if (!tail) {
            int baseDepth = frames.size();
            Frame bottom = cont.frames().get(0);
            bottom.returnDsts = ReturnDsts.fromOption(dst);
            for (HandlerEntry h : cont.handlers()) {
                h.ownerDepth = h.ownerDepth + baseDepth;
            }
            frames.addAll(cont.frames());
            handlers.addAll(cont.handlers());
            return null;
        }

        // Tail resume: replace current frame.
        int baseDepth = frameIndex;
        ReturnDsts returnDsts = frame.returnDsts;
        Frame bottom = cont.frames().get(0);
        bottom.returnDsts = returnDsts;

        for (HandlerEntry h : cont.handlers()) {
            h.ownerDepth = h.ownerDepth + baseDepth;
        }

        frames.subList(baseDepth, frames.size()).clear();
        unwindHandlersToStackLen(baseDepth);

        frames.addAll(cont.frames());
        handlers.addAll(cont.handlers());
        return null;
    }

    // ====== Pattern matching ======

    private boolean matchPattern(Pattern pat, Value value, ArrayList<Value> binds) throws VmTrap {
        if (pat instanceof Pattern.Wildcard) {
            return true;
        }
        if (pat instanceof Pattern.Bind) {
            binds.add(value);
            return true;
        }
        if (pat instanceof Pattern.Literal lit) {
            return matchLiteral(lit.value(), value);
        }
        if (pat instanceof Pattern.Enum p) {
            if (!(value instanceof Value.Ref ref)) {
                return false;
            }
            HeapObject obj = ref.value().obj();
            if (!(obj instanceof EnumObj e)) {
                return false;
            }
            TypeId patEnumId = module.typeId(p.enumName()).orElse(null);
            if (patEnumId == null) {
                return false;
            }
            if (!e.typeId.equals(patEnumId) || !e.variant.equals(p.variant()) || e.fields.size() != p.fields().size()) {
                return false;
            }
            for (int i = 0; i < p.fields().size(); i++) {
                Value actual = e.fields.get(i);
                if (ref.value().readonly()) {
                    actual = actual.intoReadonlyView();
                }
                if (!matchPattern(p.fields().get(i), actual, binds)) {
                    return false;
                }
            }
            return true;
        }
        if (pat instanceof Pattern.Tuple p) {
            if (!(value instanceof Value.Ref ref)) {
                return false;
            }
            HeapObject obj = ref.value().obj();
            if (!(obj instanceof TupleObj t)) {
                return false;
            }
            int minLen = p.prefix().size() + p.suffix().size();
            if (p.rest() != null) {
                if (t.items.size() < minLen) {
                    return false;
                }
            } else if (t.items.size() != minLen) {
                return false;
            }

            boolean readonly = ref.value().readonly();

            for (int i = 0; i < p.prefix().size(); i++) {
                Value actual = t.items.get(i);
                if (readonly) {
                    actual = actual.intoReadonlyView();
                }
                if (!matchPattern(p.prefix().get(i), actual, binds)) {
                    return false;
                }
            }

            if (p.rest() != null) {
                int start = p.prefix().size();
                int end = t.items.size() - p.suffix().size();
                ArrayList<Value> slice = new ArrayList<>(end - start);
                for (int i = start; i < end; i++) {
                    Value v = t.items.get(i);
                    slice.add(readonly ? v.intoReadonlyView() : v);
                }

                if (p.rest() instanceof Pattern.Wildcard) {
                    // ignore
                } else if (p.rest() instanceof Pattern.Bind) {
                    if (slice.isEmpty()) {
                        binds.add(new Value.Unit());
                    } else {
                        binds.add(allocRef(new TupleObj(slice)));
                    }
                } else {
                    return false;
                }
            }

            for (int i = 0; i < p.suffix().size(); i++) {
                Value actual = t.items.get(t.items.size() - p.suffix().size() + i);
                if (readonly) {
                    actual = actual.intoReadonlyView();
                }
                if (!matchPattern(p.suffix().get(i), actual, binds)) {
                    return false;
                }
            }
            return true;
        }
        if (pat instanceof Pattern.Struct p) {
            if (!(value instanceof Value.Ref ref)) {
                return false;
            }
            HeapObject obj = ref.value().obj();
            if (!(obj instanceof StructObj s)) {
                return false;
            }
            TypeId patTypeId = module.typeId(p.typeName()).orElse(null);
            if (patTypeId == null) {
                return false;
            }
            if (!s.typeId.equals(patTypeId)) {
                return false;
            }
            for (Pattern.Struct.Field f : p.fields()) {
                int idx = structFieldIndex(s.typeId, f.name());
                if (idx >= s.fields.size()) {
                    throw new VmTrap("missing field: " + f.name());
                }
                Value fieldValue = s.fields.get(idx);
                if (ref.value().readonly()) {
                    fieldValue = fieldValue.intoReadonlyView();
                }
                if (!matchPattern(f.pattern(), fieldValue, binds)) {
                    return false;
                }
            }
            return true;
        }
        if (pat instanceof Pattern.Array p) {
            if (!(value instanceof Value.Ref ref)) {
                return false;
            }
            HeapObject obj = ref.value().obj();
            if (!(obj instanceof ArrayObj arr)) {
                return false;
            }
            int minLen = p.prefix().size() + p.suffix().size();
            if (p.rest() != null) {
                if (arr.items.size() < minLen) {
                    return false;
                }
            } else if (arr.items.size() != minLen) {
                return false;
            }

            boolean readonly = ref.value().readonly();

            for (int i = 0; i < p.prefix().size(); i++) {
                Value actual = arr.items.get(i);
                if (readonly) {
                    actual = actual.intoReadonlyView();
                }
                if (!matchPattern(p.prefix().get(i), actual, binds)) {
                    return false;
                }
            }

            if (p.rest() != null) {
                int start = p.prefix().size();
                int end = arr.items.size() - p.suffix().size();
                ArrayList<Value> slice = new ArrayList<>(end - start);
                for (int i = start; i < end; i++) {
                    Value v = arr.items.get(i);
                    slice.add(readonly ? v.intoReadonlyView() : v);
                }

                if (p.rest() instanceof Pattern.Wildcard) {
                    // ignore
                } else if (p.rest() instanceof Pattern.Bind) {
                    binds.add(allocRef(new ArrayObj(slice)));
                } else {
                    return false;
                }
            }

            for (int i = 0; i < p.suffix().size(); i++) {
                Value actual = arr.items.get(arr.items.size() - p.suffix().size() + i);
                if (readonly) {
                    actual = actual.intoReadonlyView();
                }
                if (!matchPattern(p.suffix().get(i), actual, binds)) {
                    return false;
                }
            }
            return true;
        }

        throw new VmTrap("unknown pattern");
    }

    private boolean matchLiteral(ConstValue lit, Value value) throws VmTrap {
        if (lit instanceof ConstValue.Unit) {
            return value instanceof Value.Unit;
        }
        if (lit instanceof ConstValue.Bool a) {
            return value instanceof Value.Bool b && a.value() == b.value();
        }
        if (lit instanceof ConstValue.Int a) {
            return value instanceof Value.Int b && a.value() == b.value();
        }
        if (lit instanceof ConstValue.Float a) {
            return value instanceof Value.Float b && Double.compare(a.value(), b.value()) == 0;
        }
        if (lit instanceof ConstValue.Str a) {
            return value instanceof Value.Str b && a.value().equals(b.value().asJavaString());
        }
        if (lit instanceof ConstValue.Bytes a) {
            return value instanceof Value.Bytes b && java.util.Arrays.equals(a.value(), b.value().toByteArray());
        }
        if (lit instanceof ConstValue.TypeRep t) {
            if (!(value instanceof Value.TypeRep tr)) {
                return false;
            }
            TypeReps.TypeRepNode node = typeReps.node(tr.value());
            if (node == null) {
                return false;
            }
            TypeReps.TypeCtor ctor;
            try {
                ctor = TypeReps.ctorFromLit(module, t.value());
            } catch (IllegalArgumentException e) {
                return false;
            }
            return node.ctor().equals(ctor) && node.args().isEmpty();
        }
        if (lit instanceof ConstValue.Function a) {
            return value instanceof Value.Function b && a.value().equals(b.value());
        }
        return false;
    }

    // ====== Intrinsics ======

    private Value evalCoreIntrinsic(Intrinsic intr, Frame frame, List<Integer> argRegs) throws VmTrap {
        ArrayList<Value> args = new ArrayList<>(argRegs.size());
        for (int r : argRegs) {
            args.add(readValue(frame, r));
        }

        String badArgs = "core::intrinsics::" + intr.name() + ": bad args: " + args;

        // Helper: intern common typereps.
        TypeReps.TypeRepId byteRep =
                typeReps.intern(new TypeReps.TypeRepNode(new TypeReps.TypeCtor.Byte(), List.of()));
        TypeReps.TypeRepId charRep =
                typeReps.intern(new TypeReps.TypeRepNode(new TypeReps.TypeCtor.Char(), List.of()));
        TypeReps.TypeRepId stringRep =
                typeReps.intern(new TypeReps.TypeRepNode(new TypeReps.TypeCtor.String(), List.of()));

        switch (intr) {
            case StringConcat -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Str a && args.get(1) instanceof Value.Str b) {
                    return new Value.Str(a.value().concat(b.value()));
                }
                throw new VmTrap(badArgs);
            }
            case ToString -> {
                if (args.size() != 2 || !(args.get(0) instanceof Value.TypeRep)) {
                    throw new VmTrap(badArgs);
                }
                Value v = args.get(1);
                if (v instanceof Value.Unit) {
                    return new Value.Str(new RuskString("()"));
                }
                if (v instanceof Value.Bool b) {
                    return new Value.Str(new RuskString(Boolean.toString(b.value())));
                }
                if (v instanceof Value.Int n) {
                    return new Value.Str(new RuskString(Long.toString(n.value())));
                }
                if (v instanceof Value.Float x) {
                    return new Value.Str(new RuskString(Double.toString(x.value())));
                }
                if (v instanceof Value.Byte b) {
                    return new Value.Str(new RuskString(Integer.toString(b.value())));
                }
                if (v instanceof Value.Char c) {
                    return new Value.Str(new RuskString(new String(Character.toChars(c.codePoint()))));
                }
                if (v instanceof Value.Str s) {
                    return v; // already a string
                }
                if (v instanceof Value.Bytes b) {
                    return new Value.Str(new RuskString("bytes(len=" + b.value().byteLen() + ")"));
                }
                if (v instanceof Value.Function f) {
                    return new Value.Str(new RuskString("fn#" + f.value().index()));
                }
                if (v instanceof Value.TypeRep t) {
                    return new Value.Str(new RuskString("typerep(" + t.value().index() + ")"));
                }
                if (v instanceof Value.Ref) {
                    return new Value.Str(new RuskString("ref"));
                }
                throw new VmTrap(badArgs);
            }
            case Panic -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Str msg) {
                    throw new VmTrap("panic: " + msg.value().asJavaString());
                }
                throw new VmTrap(badArgs);
            }

            case BoolNot -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Bool b) {
                    return new Value.Bool(!b.value());
                }
                throw new VmTrap(badArgs);
            }
            case BoolEq -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Bool a && args.get(1) instanceof Value.Bool b) {
                    return new Value.Bool(a.value() == b.value());
                }
                throw new VmTrap(badArgs);
            }
            case BoolNe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Bool a && args.get(1) instanceof Value.Bool b) {
                    return new Value.Bool(a.value() != b.value());
                }
                throw new VmTrap(badArgs);
            }

            case IntAdd -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Int(a.value() + b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntSub -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Int(a.value() - b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntMul -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Int(a.value() * b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntDiv -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    if (b.value() == 0) {
                        throw new VmTrap("core::intrinsics::int_div: division by zero");
                    }
                    return new Value.Int(a.value() / b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntMod -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    if (b.value() == 0) {
                        throw new VmTrap("core::intrinsics::int_mod: modulo by zero");
                    }
                    return new Value.Int(a.value() % b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntAnd -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Int(a.value() & b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntOr -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Int(a.value() | b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntXor -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Int(a.value() ^ b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntNot -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Int v) {
                    return new Value.Int(~v.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntShl -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int sh) {
                    if (sh.value() < 0 || sh.value() >= 64) {
                        throw new VmTrap("core::intrinsics::int_shl: shift amount out of range");
                    }
                    return new Value.Int(a.value() << (int) sh.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntShr -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int sh) {
                    if (sh.value() < 0 || sh.value() >= 64) {
                        throw new VmTrap("core::intrinsics::int_shr: shift amount out of range");
                    }
                    return new Value.Int(a.value() >> (int) sh.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntUShr -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int sh) {
                    if (sh.value() < 0 || sh.value() >= 64) {
                        throw new VmTrap("core::intrinsics::int_ushr: shift amount out of range");
                    }
                    return new Value.Int(a.value() >>> (int) sh.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntEq -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Bool(a.value() == b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntNe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Bool(a.value() != b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntLt -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Bool(a.value() < b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntLe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Bool(a.value() <= b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntGt -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Bool(a.value() > b.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntGe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    return new Value.Bool(a.value() >= b.value());
                }
                throw new VmTrap(badArgs);
            }

            case FloatAdd -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Float(a.value() + b.value());
                }
                throw new VmTrap(badArgs);
            }
            case FloatSub -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Float(a.value() - b.value());
                }
                throw new VmTrap(badArgs);
            }
            case FloatMul -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Float(a.value() * b.value());
                }
                throw new VmTrap(badArgs);
            }
            case FloatDiv -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Float(a.value() / b.value());
                }
                throw new VmTrap(badArgs);
            }
            case FloatMod -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Float(a.value() % b.value());
                }
                throw new VmTrap(badArgs);
            }
            case FloatEq -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Bool(Double.compare(a.value(), b.value()) == 0);
                }
                throw new VmTrap(badArgs);
            }
            case FloatNe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Bool(Double.compare(a.value(), b.value()) != 0);
                }
                throw new VmTrap(badArgs);
            }
            case FloatLt -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Bool(a.value() < b.value());
                }
                throw new VmTrap(badArgs);
            }
            case FloatLe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Bool(a.value() <= b.value());
                }
                throw new VmTrap(badArgs);
            }
            case FloatGt -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Bool(a.value() > b.value());
                }
                throw new VmTrap(badArgs);
            }
            case FloatGe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Float a && args.get(1) instanceof Value.Float b) {
                    return new Value.Bool(a.value() >= b.value());
                }
                throw new VmTrap(badArgs);
            }

            case StringEq -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Str a && args.get(1) instanceof Value.Str b) {
                    return new Value.Bool(a.value().equals(b.value()));
                }
                throw new VmTrap(badArgs);
            }
            case StringNe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Str a && args.get(1) instanceof Value.Str b) {
                    return new Value.Bool(!a.value().equals(b.value()));
                }
                throw new VmTrap(badArgs);
            }
            case BytesEq -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Bytes a && args.get(1) instanceof Value.Bytes b) {
                    return new Value.Bool(a.value().equals(b.value()));
                }
                throw new VmTrap(badArgs);
            }
            case BytesNe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Bytes a && args.get(1) instanceof Value.Bytes b) {
                    return new Value.Bool(!a.value().equals(b.value()));
                }
                throw new VmTrap(badArgs);
            }
            case UnitEq -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Unit && args.get(1) instanceof Value.Unit) {
                    return new Value.Bool(true);
                }
                throw new VmTrap(badArgs);
            }
            case UnitNe -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Unit && args.get(1) instanceof Value.Unit) {
                    return new Value.Bool(false);
                }
                throw new VmTrap(badArgs);
            }

            case HashInt -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Int v) {
                    long hash = fnv1a64(leBytes(v.value()));
                    return new Value.Int(hash);
                }
                throw new VmTrap(badArgs);
            }
            case HashString -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Str s) {
                    long hash = fnv1a64(s.value().toUtf8Bytes());
                    return new Value.Int(hash);
                }
                throw new VmTrap(badArgs);
            }
            case HashBytes -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Bytes b) {
                    long hash = fnv1a64(b.value().toByteArray());
                    return new Value.Int(hash);
                }
                throw new VmTrap(badArgs);
            }
            case HashCombine -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Int a && args.get(1) instanceof Value.Int b) {
                    long hash = fnv1a64(concat(leBytes(a.value()), leBytes(b.value())));
                    return new Value.Int(hash);
                }
                throw new VmTrap(badArgs);
            }

            case ArrayLen, ArrayLenRo -> {
                if (args.size() == 2 && args.get(0) instanceof Value.TypeRep && args.get(1) instanceof Value.Ref arr) {
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_len: expected an array");
                    }
                    return new Value.Int(a.items.size());
                }
                throw new VmTrap(badArgs);
            }
            case ArrayPush -> {
                if (args.size() == 3 && args.get(0) instanceof Value.TypeRep && args.get(1) instanceof Value.Ref arr) {
                    if (arr.value().readonly()) {
                        throw new VmTrap("illegal write through readonly reference");
                    }
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_push: expected an array");
                    }
                    a.items.add(args.get(2));
                    return new Value.Unit();
                }
                throw new VmTrap(badArgs);
            }
            case ArrayPop -> {
                if (args.size() == 2 && args.get(0) instanceof Value.TypeRep elemRep && args.get(1) instanceof Value.Ref arr) {
                    if (arr.value().readonly()) {
                        throw new VmTrap("illegal write through readonly reference");
                    }
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_pop: expected an array");
                    }
                    if (a.items.isEmpty()) {
                        return optionNone(elemRep.value());
                    }
                    Value popped = a.items.remove(a.items.size() - 1);
                    return optionSome(elemRep.value(), popped);
                }
                throw new VmTrap(badArgs);
            }
            case ArrayClear -> {
                if (args.size() == 2 && args.get(0) instanceof Value.TypeRep && args.get(1) instanceof Value.Ref arr) {
                    if (arr.value().readonly()) {
                        throw new VmTrap("illegal write through readonly reference");
                    }
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_clear: expected an array");
                    }
                    a.items.clear();
                    return new Value.Unit();
                }
                throw new VmTrap(badArgs);
            }
            case ArrayResize -> {
                if (args.size() == 4
                        && args.get(0) instanceof Value.TypeRep
                        && args.get(1) instanceof Value.Ref arr
                        && args.get(2) instanceof Value.Int newLen) {
                    if (arr.value().readonly()) {
                        throw new VmTrap("illegal write through readonly reference");
                    }
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_resize: expected an array");
                    }
                    if (newLen.value() < 0) {
                        throw new VmTrap("core::intrinsics::array_resize: new_len must be >= 0");
                    }
                    int n = (int) newLen.value();
                    if (n <= a.items.size()) {
                        while (a.items.size() > n) {
                            a.items.remove(a.items.size() - 1);
                        }
                        return new Value.Unit();
                    }
                    Value fill = args.get(3);
                    while (a.items.size() < n) {
                        a.items.add(fill);
                    }
                    return new Value.Unit();
                }
                throw new VmTrap(badArgs);
            }
            case ArrayInsert -> {
                if (args.size() == 4
                        && args.get(0) instanceof Value.TypeRep
                        && args.get(1) instanceof Value.Ref arr
                        && args.get(2) instanceof Value.Int idx) {
                    if (arr.value().readonly()) {
                        throw new VmTrap("illegal write through readonly reference");
                    }
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_insert: expected an array");
                    }
                    long i = idx.value();
                    if (i < 0 || i > Integer.MAX_VALUE) {
                        throw new VmTrap("index out of bounds: index=" + i + ", len=" + a.items.size());
                    }
                    int ii = (int) i;
                    if (ii > a.items.size()) {
                        throw new VmTrap("index out of bounds: index=" + i + ", len=" + a.items.size());
                    }
                    a.items.add(ii, args.get(3));
                    return new Value.Unit();
                }
                throw new VmTrap(badArgs);
            }
            case ArrayRemove -> {
                if (args.size() == 3
                        && args.get(0) instanceof Value.TypeRep
                        && args.get(1) instanceof Value.Ref arr
                        && args.get(2) instanceof Value.Int idx) {
                    if (arr.value().readonly()) {
                        throw new VmTrap("illegal write through readonly reference");
                    }
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_remove: expected an array");
                    }
                    long i = idx.value();
                    if (i < 0 || i > Integer.MAX_VALUE) {
                        throw new VmTrap("index out of bounds: index=" + i + ", len=" + a.items.size());
                    }
                    int ii = (int) i;
                    if (ii >= a.items.size()) {
                        throw new VmTrap("index out of bounds: index=" + i + ", len=" + a.items.size());
                    }
                    return a.items.remove(ii);
                }
                throw new VmTrap(badArgs);
            }
            case ArrayExtend -> {
                if (args.size() == 3
                        && args.get(0) instanceof Value.TypeRep
                        && args.get(1) instanceof Value.Ref arr
                        && args.get(2) instanceof Value.Ref other) {
                    if (arr.value().readonly()) {
                        throw new VmTrap("illegal write through readonly reference");
                    }
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_extend: expected an array for `arr`");
                    }
                    if (!(other.value().obj() instanceof ArrayObj b)) {
                        throw new VmTrap("core::intrinsics::array_extend: expected an array for `other`");
                    }
                    a.items.addAll(b.items);
                    return new Value.Unit();
                }
                throw new VmTrap(badArgs);
            }
            case ArrayConcat, ArrayConcatRo -> {
                if (args.size() == 3
                        && args.get(0) instanceof Value.TypeRep
                        && args.get(1) instanceof Value.Ref a
                        && args.get(2) instanceof Value.Ref b) {
                    if (!(a.value().obj() instanceof ArrayObj aa)) {
                        throw new VmTrap("core::intrinsics::array_concat: expected an array for `a`");
                    }
                    if (!(b.value().obj() instanceof ArrayObj bb)) {
                        throw new VmTrap("core::intrinsics::array_concat: expected an array for `b`");
                    }
                    ArrayList<Value> items = new ArrayList<>(aa.items.size() + bb.items.size());
                    if (intr == Intrinsic.ArrayConcatRo) {
                        for (Value v : aa.items) {
                            items.add(v.intoReadonlyView());
                        }
                        for (Value v : bb.items) {
                            items.add(v.intoReadonlyView());
                        }
                    } else {
                        items.addAll(aa.items);
                        items.addAll(bb.items);
                    }
                    return allocRef(new ArrayObj(items));
                }
                throw new VmTrap(badArgs);
            }
            case ArraySlice, ArraySliceRo -> {
                if (args.size() == 4
                        && args.get(0) instanceof Value.TypeRep
                        && args.get(1) instanceof Value.Ref arr
                        && args.get(2) instanceof Value.Int start
                        && args.get(3) instanceof Value.Int end) {
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::array_slice: expected an array");
                    }
                    int len = a.items.size();
                    long s = start.value();
                    long e = end.value();
                    if (s < 0 || s > Integer.MAX_VALUE) {
                        throw new VmTrap("index out of bounds: index=" + s + ", len=" + len);
                    }
                    if (e < 0 || e > Integer.MAX_VALUE) {
                        throw new VmTrap("index out of bounds: index=" + e + ", len=" + len);
                    }
                    int ss = (int) s;
                    int ee = (int) e;
                    if (ss > ee) {
                        throw new VmTrap("core::intrinsics::array_slice: start must be <= end");
                    }
                    if (ee > len) {
                        throw new VmTrap("index out of bounds: index=" + e + ", len=" + len);
                    }
                    ArrayList<Value> slice = new ArrayList<>(ee - ss);
                    for (int i = ss; i < ee; i++) {
                        Value v = a.items.get(i);
                        slice.add(intr == Intrinsic.ArraySliceRo ? v.intoReadonlyView() : v);
                    }
                    return allocRef(new ArrayObj(slice));
                }
                throw new VmTrap(badArgs);
            }

            case IntToByte -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Int v) {
                    int b = (int) (v.value() & 0xFF);
                    return new Value.Byte(b);
                }
                throw new VmTrap(badArgs);
            }
            case IntTryByte -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Int v) {
                    if (0 <= v.value() && v.value() <= 255) {
                        return optionSome(byteRep, new Value.Byte((int) v.value()));
                    }
                    return optionNone(byteRep);
                }
                throw new VmTrap(badArgs);
            }
            case ByteToInt -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Byte v) {
                    return new Value.Int(v.value());
                }
                throw new VmTrap(badArgs);
            }
            case ByteAnd -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Byte a && args.get(1) instanceof Value.Byte b) {
                    return new Value.Byte(a.value() & b.value());
                }
                throw new VmTrap(badArgs);
            }
            case ByteOr -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Byte a && args.get(1) instanceof Value.Byte b) {
                    return new Value.Byte(a.value() | b.value());
                }
                throw new VmTrap(badArgs);
            }
            case ByteXor -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Byte a && args.get(1) instanceof Value.Byte b) {
                    return new Value.Byte(a.value() ^ b.value());
                }
                throw new VmTrap(badArgs);
            }
            case ByteNot -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Byte v) {
                    return new Value.Byte((~v.value()) & 0xFF);
                }
                throw new VmTrap(badArgs);
            }
            case ByteShl -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Byte a && args.get(1) instanceof Value.Int sh) {
                    if (sh.value() < 0 || sh.value() >= 8) {
                        throw new VmTrap("core::intrinsics::byte_shl: shift amount out of range");
                    }
                    return new Value.Byte((a.value() << (int) sh.value()) & 0xFF);
                }
                throw new VmTrap(badArgs);
            }
            case ByteShr -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Byte a && args.get(1) instanceof Value.Int sh) {
                    if (sh.value() < 0 || sh.value() >= 8) {
                        throw new VmTrap("core::intrinsics::byte_shr: shift amount out of range");
                    }
                    return new Value.Byte(a.value() >>> (int) sh.value());
                }
                throw new VmTrap(badArgs);
            }
            case ByteUShr -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Byte a && args.get(1) instanceof Value.Int sh) {
                    if (sh.value() < 0 || sh.value() >= 8) {
                        throw new VmTrap("core::intrinsics::byte_ushr: shift amount out of range");
                    }
                    return new Value.Byte(a.value() >>> (int) sh.value());
                }
                throw new VmTrap(badArgs);
            }
            case IntToChar -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Int v) {
                    long n = v.value();
                    if (n < 0 || n > 0x10FFFFL) {
                        throw new VmTrap("core::intrinsics::int_to_char: value out of range");
                    }
                    int cp = (int) n;
                    if (0xD800 <= cp && cp <= 0xDFFF) {
                        throw new VmTrap("core::intrinsics::int_to_char: surrogate code point");
                    }
                    if (!Character.isValidCodePoint(cp)) {
                        throw new VmTrap("core::intrinsics::int_to_char: invalid scalar value");
                    }
                    return new Value.Char(cp);
                }
                throw new VmTrap(badArgs);
            }
            case IntTryChar -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Int v) {
                    long n = v.value();
                    if (0 <= n && n <= 0x10FFFFL) {
                        int cp = (int) n;
                        if (!(0xD800 <= cp && cp <= 0xDFFF) && Character.isValidCodePoint(cp)) {
                            return optionSome(charRep, new Value.Char(cp));
                        }
                    }
                    return optionNone(charRep);
                }
                throw new VmTrap(badArgs);
            }
            case CharToInt -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Char c) {
                    return new Value.Int(c.codePoint());
                }
                throw new VmTrap(badArgs);
            }

            case BytesGet -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Bytes b && args.get(1) instanceof Value.Int idx) {
                    long i = idx.value();
                    if (i < 0 || i > Integer.MAX_VALUE) {
                        return optionNone(byteRep);
                    }
                    int ii = (int) i;
                    if (ii >= b.value().byteLen()) {
                        return optionNone(byteRep);
                    }
                    int byteValue = b.value().byteAt(ii) & 0xFF;
                    return optionSome(byteRep, new Value.Byte(byteValue));
                }
                throw new VmTrap(badArgs);
            }
            case BytesLen -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Bytes b) {
                    return new Value.Int(b.value().byteLen());
                }
                throw new VmTrap(badArgs);
            }
            case BytesSlice -> {
                if (args.size() == 3 && args.get(0) instanceof Value.Bytes b && args.get(1) instanceof Value.Int from) {
                    long len = b.value().byteLen();
                    Optional<Long> toOpt = readOptionInt(args.get(2));
                    long to = toOpt.orElse(len);

                    if (from.value() < 0) {
                        throw new VmTrap("core::intrinsics::bytes_slice: from must be >= 0");
                    }
                    if (to < 0) {
                        throw new VmTrap("core::intrinsics::bytes_slice: to must be >= 0");
                    }
                    if (from.value() > to) {
                        throw new VmTrap("core::intrinsics::bytes_slice: from > to");
                    }
                    if (to > len) {
                        throw new VmTrap("core::intrinsics::bytes_slice: to out of bounds");
                    }

                    return new Value.Bytes(b.value().slice((int) from.value(), (int) to));
                }
                throw new VmTrap(badArgs);
            }
            case BytesToArray -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Bytes b) {
                    ArrayList<Value> items = new ArrayList<>(b.value().byteLen());
                    for (int i = 0; i < b.value().byteLen(); i++) {
                        items.add(new Value.Byte(b.value().byteAt(i) & 0xFF));
                    }
                    return allocRef(new ArrayObj(items));
                }
                throw new VmTrap(badArgs);
            }
            case BytesFromArray -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Ref arr) {
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::bytes_from_array: expected an array");
                    }
                    byte[] out = new byte[a.items.size()];
                    for (int i = 0; i < a.items.size(); i++) {
                        Value item = a.items.get(i);
                        if (!(item instanceof Value.Byte b)) {
                            throw new VmTrap("core::intrinsics::bytes_from_array: expected `[byte]`");
                        }
                        out[i] = (byte) (b.value() & 0xFF);
                    }
                    return new Value.Bytes(new RuskBytes(out));
                }
                throw new VmTrap(badArgs);
            }

            case StringSlice -> {
                if (args.size() == 3 && args.get(0) instanceof Value.Str s && args.get(1) instanceof Value.Int from) {
                    Optional<Long> toOpt = readOptionInt(args.get(2));
                    RuskString rs = s.value();

                    if (from.value() < 0) {
                        throw new VmTrap("core::intrinsics::string_slice: from must be >= 0");
                    }
                    if (toOpt.isPresent() && toOpt.get() < 0) {
                        throw new VmTrap("core::intrinsics::string_slice: to must be >= 0");
                    }

                    if (from.value() > Integer.MAX_VALUE) {
                        throw new VmTrap("core::intrinsics::string_slice: from overflow");
                    }
                    int fromCp = (int) from.value();

                    Integer toCp = null;
                    if (toOpt.isPresent()) {
                        long toLong = toOpt.get();
                        if (toLong > Integer.MAX_VALUE) {
                            throw new VmTrap("core::intrinsics::string_slice: to overflow");
                        }
                        toCp = (int) toLong;
                    }

                    if (toCp != null && fromCp > toCp) {
                        throw new VmTrap("core::intrinsics::string_slice: from > to");
                    }

                    // Convert codepoint indices into UTF-8 byte offsets.
                    Integer fromByte = (fromCp == 0) ? 0 : null;
                    Integer toByte = (toCp != null && toCp == 0) ? 0 : null;
                    int cpIdx = 0;
                    int byteIdx = 0;
                    while (byteIdx < rs.byteLen()) {
                        if (fromByte == null && cpIdx == fromCp) {
                            fromByte = byteIdx;
                        }
                        if (toCp != null && toByte == null && cpIdx == toCp) {
                            toByte = byteIdx;
                        }
                        try {
                            byteIdx = rs.decodeNextChar(byteIdx).nextByteIndex();
                        } catch (CharacterCodingException e) {
                            throw new VmTrap("core::intrinsics::string_slice: invalid UTF-8");
                        }
                        cpIdx += 1;
                    }
                    int cpLen = cpIdx;

                    if (fromByte == null) {
                        if (fromCp == cpLen) {
                            fromByte = rs.byteLen();
                        } else {
                            throw new VmTrap("core::intrinsics::string_slice: from out of bounds");
                        }
                    }

                    int toByteFinal;
                    if (toCp == null) {
                        toByteFinal = rs.byteLen();
                    } else if (toByte != null) {
                        toByteFinal = toByte;
                    } else {
                        if (toCp == cpLen) {
                            toByteFinal = rs.byteLen();
                        } else {
                            throw new VmTrap("core::intrinsics::string_slice: to out of bounds");
                        }
                    }

                    try {
                        return new Value.Str(rs.slice(fromByte, toByteFinal));
                    } catch (CharacterCodingException e) {
                        throw new VmTrap("core::intrinsics::string_slice: invalid UTF-8 boundary");
                    }
                }
                throw new VmTrap(badArgs);
            }
            case StringByteSlice -> {
                if (args.size() == 3 && args.get(0) instanceof Value.Str s && args.get(1) instanceof Value.Int from) {
                    long len = s.value().byteLen();
                    Optional<Long> toOpt = readOptionInt(args.get(2));
                    long to = toOpt.orElse(len);

                    if (from.value() < 0) {
                        throw new VmTrap("core::intrinsics::string_byte_slice: from must be >= 0");
                    }
                    if (to < 0) {
                        throw new VmTrap("core::intrinsics::string_byte_slice: to must be >= 0");
                    }
                    if (from.value() > to) {
                        throw new VmTrap("core::intrinsics::string_byte_slice: from > to");
                    }
                    if (to > len) {
                        throw new VmTrap("core::intrinsics::string_byte_slice: to out of bounds");
                    }
                    if (from.value() > Integer.MAX_VALUE || to > Integer.MAX_VALUE) {
                        throw new VmTrap("core::intrinsics::string_byte_slice: index overflow");
                    }

                    return new Value.Bytes(s.value().sliceBytes((int) from.value(), (int) to));
                }
                throw new VmTrap(badArgs);
            }
            case StringNextIndex -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Str s && args.get(1) instanceof Value.Int idx) {
                    if (idx.value() < 0) {
                        throw new VmTrap("core::intrinsics::string_next_index: idx must be >= 0");
                    }
                    if (idx.value() > Integer.MAX_VALUE) {
                        throw new VmTrap("core::intrinsics::string_next_index: idx overflow");
                    }
                    int i = (int) idx.value();
                    int len = s.value().byteLen();
                    if (i > len) {
                        throw new VmTrap("core::intrinsics::string_next_index: idx out of bounds");
                    }
                    if (i == len) {
                        return new Value.Int(-1);
                    }
                    if (!s.value().isCharBoundary(i)) {
                        throw new VmTrap("core::intrinsics::string_next_index: invalid UTF-8 boundary");
                    }
                    try {
                        return new Value.Int(s.value().decodeNextChar(i).nextByteIndex());
                    } catch (CharacterCodingException e) {
                        throw new VmTrap("core::intrinsics::string_next_index: invalid UTF-8");
                    }
                }
                throw new VmTrap(badArgs);
            }
            case StringCodepointAt -> {
                if (args.size() == 2 && args.get(0) instanceof Value.Str s && args.get(1) instanceof Value.Int idx) {
                    if (idx.value() < 0) {
                        throw new VmTrap("core::intrinsics::string_codepoint_at: idx must be >= 0");
                    }
                    if (idx.value() > Integer.MAX_VALUE) {
                        throw new VmTrap("core::intrinsics::string_codepoint_at: idx overflow");
                    }
                    int i = (int) idx.value();
                    int len = s.value().byteLen();
                    if (i >= len) {
                        throw new VmTrap("core::intrinsics::string_codepoint_at: idx out of bounds");
                    }
                    if (!s.value().isCharBoundary(i)) {
                        throw new VmTrap("core::intrinsics::string_codepoint_at: invalid UTF-8 boundary");
                    }
                    try {
                        return new Value.Int(s.value().decodeNextChar(i).codePoint());
                    } catch (CharacterCodingException e) {
                        throw new VmTrap("core::intrinsics::string_codepoint_at: invalid UTF-8");
                    }
                }
                throw new VmTrap(badArgs);
            }
            case StringFromChars -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Ref arr) {
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::string_from_chars: expected an array");
                    }
                    StringBuilder sb = new StringBuilder();
                    for (Value item : a.items) {
                        if (!(item instanceof Value.Char c)) {
                            throw new VmTrap("core::intrinsics::string_from_chars: expected `[char]`");
                        }
                        sb.appendCodePoint(c.codePoint());
                    }
                    return new Value.Str(new RuskString(sb.toString()));
                }
                throw new VmTrap(badArgs);
            }
            case StringFromUtf8 -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Bytes b) {
                    return new Value.Str(RuskString.fromUtf8BytesLossy(b.value().toByteArray()));
                }
                throw new VmTrap(badArgs);
            }
            case StringFromUtf8Strict -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Bytes b) {
                    Optional<RuskString> maybe = RuskString.tryFromUtf8BytesStrict(b.value().toByteArray());
                    if (maybe.isPresent()) {
                        return optionSome(stringRep, new Value.Str(maybe.get()));
                    }
                    return optionNone(stringRep);
                }
                throw new VmTrap(badArgs);
            }
            case StringFromUtf16Le, StringFromUtf16Be -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Ref arr) {
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::string_from_utf16: expected an array");
                    }
                    int[] units = new int[a.items.size()];
                    for (int i = 0; i < a.items.size(); i++) {
                        Value item = a.items.get(i);
                        if (item instanceof Value.Int n && 0 <= n.value() && n.value() <= 0xFFFFL) {
                            units[i] = (int) n.value();
                        } else {
                            units[i] = 0xFFFD;
                        }
                    }
                    String out = decodeUtf16Lossy(units);
                    return new Value.Str(new RuskString(out));
                }
                throw new VmTrap(badArgs);
            }
            case StringFromUtf16LeStrict, StringFromUtf16BeStrict -> {
                if (args.size() == 1 && args.get(0) instanceof Value.Ref arr) {
                    if (!(arr.value().obj() instanceof ArrayObj a)) {
                        throw new VmTrap("core::intrinsics::string_from_utf16_strict: expected an array");
                    }
                    int[] units = new int[a.items.size()];
                    for (int i = 0; i < a.items.size(); i++) {
                        Value item = a.items.get(i);
                        if (!(item instanceof Value.Int n) || n.value() < 0 || n.value() > 0xFFFFL) {
                            return optionNone(stringRep);
                        }
                        units[i] = (int) n.value();
                    }
                    String out = decodeUtf16Strict(units);
                    if (out == null) {
                        return optionNone(stringRep);
                    }
                    return optionSome(stringRep, new Value.Str(new RuskString(out)));
                }
                throw new VmTrap(badArgs);
            }
        }

        throw new VmTrap("unimplemented intrinsic: " + intr);
    }

    private Value optionSome(TypeReps.TypeRepId typeArg, Value v) throws VmTrap {
        TypeId optionTypeId = module.typeId("Option").orElse(null);
        if (optionTypeId == null) {
            throw new VmTrap("missing required enum type `Option`");
        }
        return allocRef(new EnumObj(optionTypeId, List.of(typeArg), "Some", new ArrayList<>(List.of(v))));
    }

    private Value optionNone(TypeReps.TypeRepId typeArg) throws VmTrap {
        TypeId optionTypeId = module.typeId("Option").orElse(null);
        if (optionTypeId == null) {
            throw new VmTrap("missing required enum type `Option`");
        }
        return allocRef(new EnumObj(optionTypeId, List.of(typeArg), "None", new ArrayList<>()));
    }

    private Optional<Long> readOptionInt(Value v) throws VmTrap {
        TypeId optionTypeId = module.typeId("Option").orElse(null);
        if (optionTypeId == null) {
            throw new VmTrap("missing required enum type `Option`");
        }
        if (!(v instanceof Value.Ref r)) {
            throw new VmTrap("expected `Option<int>` value");
        }
        if (!(r.value().obj() instanceof EnumObj e)) {
            throw new VmTrap("expected `Option<int>` enum value");
        }
        if (!e.typeId.equals(optionTypeId)) {
            String got = module.typeName(e.typeId).orElse("<unknown>");
            throw new VmTrap("expected `Option`, got `" + got + "`");
        }
        return switch (e.variant) {
            case "None" -> Optional.empty();
            case "Some" -> {
                if (e.fields.size() == 1 && e.fields.get(0) instanceof Value.Int n) {
                    yield Optional.of(n.value());
                }
                throw new VmTrap("malformed `Option::Some(int)` value");
            }
            default -> throw new VmTrap("invalid `Option` variant `" + e.variant + "`");
        };
    }

    private static byte[] leBytes(long v) {
        byte[] out = new byte[8];
        for (int i = 0; i < 8; i++) {
            out[i] = (byte) (v >>> (8 * i));
        }
        return out;
    }

    private static byte[] concat(byte[] a, byte[] b) {
        byte[] out = new byte[a.length + b.length];
        System.arraycopy(a, 0, out, 0, a.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
    }

    private static long fnv1a64(byte[] bytes) {
        long hash = 0xcbf29ce484222325L;
        for (byte b : bytes) {
            hash ^= (b & 0xFFL);
            hash = hash * 0x100000001b3L;
        }
        return hash;
    }

    private static String decodeUtf16Lossy(int[] units) {
        StringBuilder out = new StringBuilder();
        int i = 0;
        while (i < units.length) {
            int u = units[i] & 0xFFFF;
            char c = (char) u;
            if (Character.isHighSurrogate(c)) {
                if (i + 1 < units.length) {
                    char c2 = (char) (units[i + 1] & 0xFFFF);
                    if (Character.isLowSurrogate(c2)) {
                        out.append(c);
                        out.append(c2);
                        i += 2;
                        continue;
                    }
                }
                out.append('\uFFFD');
                i += 1;
                continue;
            }
            if (Character.isLowSurrogate(c)) {
                out.append('\uFFFD');
                i += 1;
                continue;
            }
            out.append(c);
            i += 1;
        }
        return out.toString();
    }

    private static String decodeUtf16Strict(int[] units) {
        StringBuilder out = new StringBuilder();
        int i = 0;
        while (i < units.length) {
            int u = units[i] & 0xFFFF;
            char c = (char) u;
            if (Character.isHighSurrogate(c)) {
                if (i + 1 >= units.length) {
                    return null;
                }
                char c2 = (char) (units[i + 1] & 0xFFFF);
                if (!Character.isLowSurrogate(c2)) {
                    return null;
                }
                out.append(c);
                out.append(c2);
                i += 2;
                continue;
            }
            if (Character.isLowSurrogate(c)) {
                return null;
            }
            out.append(c);
            i += 1;
        }
        return out.toString();
    }
}
