package dev.rusk.examples;

import dev.rusk.bytecode.AbiType;
import dev.rusk.bytecode.CallTarget;
import dev.rusk.bytecode.ConstValue;
import dev.rusk.bytecode.ExecutableModule;
import dev.rusk.bytecode.Function;
import dev.rusk.bytecode.HostFnSig;
import dev.rusk.bytecode.HostImport;
import dev.rusk.bytecode.HostImportId;
import dev.rusk.bytecode.Instruction;
import dev.rusk.vm.AbiValue;
import dev.rusk.vm.HostImportRegistry;
import dev.rusk.vm.StepResult;
import dev.rusk.vm.Vm;
import java.util.List;

public final class EmbedHostImport {
    public static void main(String[] args) throws Exception {
        ExecutableModule module = buildModule();

        HostImportId addId =
                module.hostImportId("math::add")
                        .orElseThrow(() -> new IllegalStateException("missing host import id"));

        HostImportRegistry hostImports = new HostImportRegistry();
        hostImports.register(
                addId,
                callArgs -> {
                    if (callArgs.size() != 2
                            || !(callArgs.get(0) instanceof AbiValue.Int a)
                            || !(callArgs.get(1) instanceof AbiValue.Int b)) {
                        throw new IllegalArgumentException("math::add: bad args: " + callArgs);
                    }
                    return new AbiValue.Int(a.value() + b.value());
                });

        Vm vm = new Vm(module, hostImports);
        StepResult r = runToCompletion(vm);

        if (r instanceof StepResult.Done done) {
            System.out.println("done: " + formatAbi(done.value()));
            return;
        }
        if (r instanceof StepResult.Trap trap) {
            throw new RuntimeException("trap: " + trap.message());
        }
        throw new IllegalStateException("unexpected StepResult: " + r);
    }

    private static ExecutableModule buildModule() {
        ExecutableModule module = new ExecutableModule();

        HostImportId addId =
                module.addHostImport(
                        new HostImport(
                                "math::add",
                                new HostFnSig(List.of(AbiType.INT, AbiType.INT), AbiType.INT)));

        var main =
                module.addFunction(
                        new Function(
                                "main",
                                3,
                                0,
                                List.of(
                                        new Instruction.Const(0, new ConstValue.Int(40)),
                                        new Instruction.Const(1, new ConstValue.Int(2)),
                                        new Instruction.Call(
                                                2,
                                                new CallTarget.Host(addId),
                                                List.of(0, 1)),
                                        new Instruction.Return(2))));
        module.setEntry(main);
        return module;
    }

    private static StepResult runToCompletion(Vm vm) {
        while (true) {
            StepResult r = vm.step(null);
            if (r instanceof StepResult.Yield) {
                continue;
            }
            return r;
        }
    }

    private static String formatAbi(AbiValue v) {
        if (v instanceof AbiValue.Unit) {
            return "unit";
        }
        if (v instanceof AbiValue.Bool b) {
            return Boolean.toString(b.value());
        }
        if (v instanceof AbiValue.Int n) {
            return Long.toString(n.value());
        }
        if (v instanceof AbiValue.Float x) {
            return Double.toString(x.value());
        }
        if (v instanceof AbiValue.Str s) {
            return s.value();
        }
        if (v instanceof AbiValue.Bytes b) {
            return "bytes(len=" + b.value().length + ")";
        }
        return v.toString();
    }
}

