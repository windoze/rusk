package dev.rusk.vm;

import dev.rusk.bytecode.AbiType;
import dev.rusk.bytecode.CallTarget;
import dev.rusk.bytecode.ConstValue;
import dev.rusk.bytecode.EffectSpec;
import dev.rusk.bytecode.ExecutableModule;
import dev.rusk.bytecode.ExternalEffectDecl;
import dev.rusk.bytecode.Function;
import dev.rusk.bytecode.HostFnSig;
import dev.rusk.bytecode.HostImport;
import dev.rusk.bytecode.HostImportId;
import dev.rusk.bytecode.Instruction;
import dev.rusk.bytecode.Rbc;
import dev.rusk.bytecode.TypeRepLit;
import java.util.List;

public final class TestRunner {
    public static void main(String[] args) throws Exception {
        testRbcRoundtrip();
        testVmDoneForTrivialProgram();
        testVmArgvInjection();
        testTypeTestNeverIsFalse();
        testHostImportCall();
        testExternalEffectRequestResume();
        testExternalEffectDrop();
        testBytesBoundaryCopy();
        testNoReentryDuringHostCall();
        System.out.println("OK");
    }

    private static void testRbcRoundtrip() throws Exception {
        ExecutableModule module = new ExecutableModule();
        var main =
                module.addFunction(
                        new Function(
                                "main",
                                1,
                                0,
                                List.of(
                                        new Instruction.Const(
                                                0, new ConstValue.TypeRep(new TypeRepLit.Never())))));
        module.setEntry(main);

        byte[] bytes = Rbc.toBytes(module);
        ExecutableModule decoded = Rbc.fromBytes(bytes);
        byte[] bytes2 = Rbc.toBytes(decoded);

        assert java.util.Arrays.equals(bytes, bytes2);
    }

    private static void testTypeTestNeverIsFalse() throws Exception {
        ExecutableModule module = new ExecutableModule();
        var main =
                module.addFunction(
                        new Function(
                                "main",
                                3,
                                0,
                                List.of(
                                        new Instruction.Const(0, new ConstValue.Unit()),
                                        new Instruction.Const(1, new ConstValue.TypeRep(new TypeRepLit.Never())),
                                        new Instruction.IsType(2, 0, 1),
                                        new Instruction.Return(2))));
        module.setEntry(main);

        Vm vm = new Vm(module, new HostImportRegistry());
        StepResult r = vm.step(null);
        assert r instanceof StepResult.Done;
        AbiValue v = ((StepResult.Done) r).value();
        assert v instanceof AbiValue.Bool;
        assert !((AbiValue.Bool) v).value();
    }

    private static void testVmDoneForTrivialProgram() throws Exception {
        ExecutableModule module = new ExecutableModule();
        var main =
                module.addFunction(
                        new Function(
                                "main",
                                0,
                                0,
                                List.of()));
        module.setEntry(main);

        Vm vm = new Vm(module, new HostImportRegistry());
        StepResult r = vm.step(null);
        assert r instanceof StepResult.Done;
        StepResult.Done done = (StepResult.Done) r;
        assert done.value() instanceof AbiValue.Unit;
    }

    private static void testHostImportCall() throws Exception {
        ExecutableModule module = new ExecutableModule();
        module.addHostImport(
                new HostImport(
                        "add",
                        new HostFnSig(List.of(AbiType.INT, AbiType.INT), AbiType.INT)));

        var main =
                module.addFunction(
                        new Function(
                                "main",
                                3,
                                0,
                                List.of(
                                        new Instruction.Const(0, new ConstValue.Int(1)),
                                        new Instruction.Const(1, new ConstValue.Int(2)),
                                        new Instruction.Call(
                                                2,
                                                new CallTarget.Host(new HostImportId(0)),
                                                List.of(0, 1)),
                                        new Instruction.Return(2))));
        module.setEntry(main);

        HostImportRegistry reg = new HostImportRegistry();
        reg.register(
                new HostImportId(0),
                args -> {
                    long a = ((AbiValue.Int) args.get(0)).value();
                    long b = ((AbiValue.Int) args.get(1)).value();
                    return new AbiValue.Int(a + b);
                });

        Vm vm = new Vm(module, reg);
        StepResult r = vm.step(null);
        assert r instanceof StepResult.Done;
        AbiValue v = ((StepResult.Done) r).value();
        assert v instanceof AbiValue.Int;
        assert ((AbiValue.Int) v).value() == 3;
    }

    private static void testVmArgvInjection() throws Exception {
        ExecutableModule module = new ExecutableModule();
        var main =
                module.addFunction(
                        new Function(
                                "main",
                                2,
                                1,
                                List.of(
                                        new Instruction.Len(1, 0),
                                        new Instruction.Return(1))));
        module.setEntry(main);

        Vm vm = new Vm(module, new HostImportRegistry(), List.of("a", "b", "c"));
        StepResult r = vm.step(null);
        assert r instanceof StepResult.Done;
        AbiValue v = ((StepResult.Done) r).value();
        assert v instanceof AbiValue.Int;
        assert ((AbiValue.Int) v).value() == 3;
    }

    private static void testExternalEffectRequestResume() throws Exception {
        ExecutableModule module = new ExecutableModule();
        module.addExternalEffect(
                new ExternalEffectDecl(
                        "io",
                        "sleep",
                        new HostFnSig(List.of(AbiType.INT), AbiType.INT)));

        var main =
                module.addFunction(
                        new Function(
                                "main",
                                2,
                                0,
                                List.of(
                                        new Instruction.Const(0, new ConstValue.Int(5)),
                                        new Instruction.Perform(
                                                1,
                                                new EffectSpec("io", List.of(), "sleep"),
                                                List.of(0)),
                                        new Instruction.Return(1))));
        module.setEntry(main);

        Vm vm = new Vm(module, new HostImportRegistry());
        StepResult r1 = vm.step(null);
        assert r1 instanceof StepResult.Request;
        StepResult.Request req = (StepResult.Request) r1;
        assert req.effectId().index() == 0;
        assert req.args().size() == 1;
        assert req.args().get(0) instanceof AbiValue.Int;
        assert ((AbiValue.Int) req.args().get(0)).value() == 5;

        // suspended state should reject step()
        StepResult rSuspended = vm.step(null);
        assert rSuspended instanceof StepResult.Trap;

        vm.resume(req.k(), new AbiValue.Int(42));
        StepResult r2 = vm.step(null);
        assert r2 instanceof StepResult.Done;
        AbiValue v = ((StepResult.Done) r2).value();
        assert v instanceof AbiValue.Int;
        assert ((AbiValue.Int) v).value() == 42;
    }

    private static void testExternalEffectDrop() throws Exception {
        ExecutableModule module = new ExecutableModule();
        module.addExternalEffect(
                new ExternalEffectDecl(
                        "io",
                        "sleep",
                        new HostFnSig(List.of(AbiType.INT), AbiType.UNIT)));

        var main =
                module.addFunction(
                        new Function(
                                "main",
                                1,
                                0,
                                List.of(
                                        new Instruction.Const(0, new ConstValue.Int(5)),
                                        new Instruction.Perform(
                                                null,
                                                new EffectSpec("io", List.of(), "sleep"),
                                                List.of(0)),
                                        new Instruction.Return(0))));
        module.setEntry(main);

        Vm vm = new Vm(module, new HostImportRegistry());
        StepResult r1 = vm.step(null);
        assert r1 instanceof StepResult.Request;
        StepResult.Request req = (StepResult.Request) r1;

        vm.dropContinuation(req.k());

        StepResult r2 = vm.step(null);
        assert r2 instanceof StepResult.Trap;
        assert ((StepResult.Trap) r2).message().equals("cancelled");
    }

    private static void testBytesBoundaryCopy() throws Exception {
        ExecutableModule module = new ExecutableModule();
        module.addHostImport(
                new HostImport(
                        "mutate",
                        new HostFnSig(List.of(AbiType.BYTES), AbiType.UNIT)));

        var main =
                module.addFunction(
                        new Function(
                                "main",
                                1,
                                0,
                                List.of(
                                        new Instruction.Const(0, new ConstValue.Bytes(new byte[] {1, 2, 3})),
                                        new Instruction.Call(
                                                null,
                                                new CallTarget.Host(new HostImportId(0)),
                                                List.of(0)),
                                        new Instruction.Return(0))));
        module.setEntry(main);

        HostImportRegistry reg = new HostImportRegistry();
        reg.register(
                new HostImportId(0),
                args -> {
                    // mutate host-side bytes view; should NOT affect VM value (VM should have copied on boundary).
                    byte[] b = ((AbiValue.Bytes) args.get(0)).value();
                    b[0] = 9;
                    return new AbiValue.Unit();
                });

        Vm vm = new Vm(module, reg);
        StepResult r = vm.step(null);
        assert r instanceof StepResult.Done;
        AbiValue v = ((StepResult.Done) r).value();
        assert v instanceof AbiValue.Bytes;
        assert java.util.Arrays.equals(((AbiValue.Bytes) v).value(), new byte[] {1, 2, 3});
    }

    private static void testNoReentryDuringHostCall() throws Exception {
        ExecutableModule module = new ExecutableModule();
        module.addHostImport(
                new HostImport(
                        "reenter",
                        new HostFnSig(List.of(), AbiType.UNIT)));

        var main =
                module.addFunction(
                        new Function(
                                "main",
                                0,
                                0,
                                List.of(
                                        new Instruction.Call(
                                                null,
                                                new CallTarget.Host(new HostImportId(0)),
                                                List.of()))));
        module.setEntry(main);

        HostImportRegistry reg = new HostImportRegistry();
        Vm[] holder = new Vm[1];
        reg.register(
                new HostImportId(0),
                args -> {
                    StepResult nested = holder[0].step(null);
                    if (nested instanceof StepResult.Trap) {
                        throw new RuntimeException("reenter blocked: " + ((StepResult.Trap) nested).message());
                    }
                    return new AbiValue.Unit();
                });

        Vm vm = new Vm(module, reg);
        holder[0] = vm;

        StepResult r = vm.step(null);
        assert r instanceof StepResult.Trap;
        assert ((StepResult.Trap) r).message().contains("host call `reenter` failed");
    }
}
