package dev.rusk.examples;

import dev.rusk.bytecode.AbiType;
import dev.rusk.bytecode.ConstValue;
import dev.rusk.bytecode.EffectSpec;
import dev.rusk.bytecode.ExecutableModule;
import dev.rusk.bytecode.ExternalEffectDecl;
import dev.rusk.bytecode.Function;
import dev.rusk.bytecode.HostFnSig;
import dev.rusk.bytecode.Instruction;
import dev.rusk.vm.AbiValue;
import dev.rusk.vm.ContinuationHandle;
import dev.rusk.vm.HostImportRegistry;
import dev.rusk.vm.StepResult;
import dev.rusk.vm.Vm;
import dev.rusk.vm.VmError;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public final class ExternalEffectAsync {
    public static void main(String[] args) throws Exception {
        Args parsed = Args.parse(args);
        if (parsed == null) {
            usage();
            System.exit(2);
            return;
        }

        ExecutableModule module = buildModule();
        Vm vm = new Vm(module, new HostImportRegistry());

        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            runWithAsyncEffect(vm, module, executor, parsed.delayMs, parsed.timeoutMs);
        } finally {
            executor.shutdownNow();
        }
    }

    private static void runWithAsyncEffect(
            Vm vm, ExecutableModule module, ExecutorService executor, long delayMs, long timeoutMs) throws VmError, InterruptedException {
        while (true) {
            StepResult r = vm.step(null);
            if (r instanceof StepResult.Yield) {
                continue;
            }
            if (r instanceof StepResult.Done done) {
                System.out.println("done: " + done.value());
                return;
            }
            if (r instanceof StepResult.Trap trap) {
                System.out.println("trap: " + trap.message());
                return;
            }
            if (r instanceof StepResult.Request req) {
                ExternalEffectDecl decl =
                        module.externalEffect(req.effectId())
                                .orElseThrow(() -> new IllegalStateException("unknown effect id " + req.effectId().index()));

                if (!decl.interfaceName().equals("Demo") || !decl.method().equals("delayed_add")) {
                    vm.dropContinuation(req.k());
                    throw new IllegalStateException(
                            "unexpected external effect request: " + decl.interfaceName() + "." + decl.method());
                }

                if (req.args().size() != 2
                        || !(req.args().get(0) instanceof AbiValue.Int a)
                        || !(req.args().get(1) instanceof AbiValue.Int b)) {
                    vm.dropContinuation(req.k());
                    throw new IllegalStateException("bad request args: " + req.args());
                }

                System.out.println("request: Demo.delayed_add(" + a.value() + ", " + b.value() + ")");
                handleRequestAsync(vm, req.k(), a.value(), b.value(), executor, delayMs, timeoutMs);
                continue;
            }

            throw new IllegalStateException("unknown StepResult: " + r);
        }
    }

    private static void handleRequestAsync(
            Vm vm,
            ContinuationHandle k,
            long a,
            long b,
            ExecutorService executor,
            long delayMs,
            long timeoutMs)
            throws VmError, InterruptedException {
        Objects.requireNonNull(vm, "vm");
        Objects.requireNonNull(k, "k");

        LinkedBlockingQueue<Completion> completions = new LinkedBlockingQueue<>();
        executor.submit(
                () -> {
                    try {
                        Thread.sleep(delayMs);
                        completions.put(new Completion.Ok(new AbiValue.Int(a + b)));
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        completions.offer(new Completion.Err("interrupted"));
                    } catch (Exception e) {
                        completions.offer(new Completion.Err(e.getMessage()));
                    }
                });

        Completion completion = completions.poll(timeoutMs, TimeUnit.MILLISECONDS);
        if (completion == null) {
            System.out.println("timeout: dropContinuation(k)");
            vm.dropContinuation(k);
            return;
        }

        if (completion instanceof Completion.Ok ok) {
            System.out.println("resume: " + ok.value());
            vm.resume(k, ok.value());
            return;
        }

        Completion.Err err = (Completion.Err) completion;
        System.out.println("handler error: " + err.message());
        vm.dropContinuation(k);
    }

    private static ExecutableModule buildModule() {
        ExecutableModule module = new ExecutableModule();

        module.addExternalEffect(
                new ExternalEffectDecl(
                        "Demo",
                        "delayed_add",
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
                                        new Instruction.Perform(
                                                2,
                                                new EffectSpec("Demo", List.of(), "delayed_add"),
                                                List.of(0, 1)),
                                        new Instruction.Return(2))));
        module.setEntry(main);
        return module;
    }

    private static void usage() {
        System.err.println("usage: ExternalEffectAsync [--delay-ms N] [--timeout-ms N]");
    }

    private static final class Args {
        long delayMs = 50;
        long timeoutMs = 1000;

        static Args parse(String[] argv) {
            Args a = new Args();
            int i = 0;
            while (i < argv.length) {
                String s = argv[i];
                switch (s) {
                    case "--delay-ms" -> {
                        if (i + 1 >= argv.length) {
                            return null;
                        }
                        a.delayMs = Long.parseLong(argv[i + 1]);
                        i += 2;
                    }
                    case "--timeout-ms" -> {
                        if (i + 1 >= argv.length) {
                            return null;
                        }
                        a.timeoutMs = Long.parseLong(argv[i + 1]);
                        i += 2;
                    }
                    default -> {
                        return null;
                    }
                }
            }
            if (a.delayMs < 0 || a.timeoutMs < 0) {
                return null;
            }
            return a;
        }
    }

    private sealed interface Completion permits Completion.Ok, Completion.Err {
        record Ok(AbiValue value) implements Completion {
            public Ok {
                Objects.requireNonNull(value, "value");
            }
        }

        record Err(String message) implements Completion {
            public Err {
                Objects.requireNonNull(message, "message");
            }
        }
    }
}
