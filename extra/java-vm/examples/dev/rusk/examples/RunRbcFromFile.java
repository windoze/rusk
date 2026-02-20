package dev.rusk.examples;

import dev.rusk.bytecode.AbiType;
import dev.rusk.bytecode.ExecutableModule;
import dev.rusk.bytecode.ExternalEffectDecl;
import dev.rusk.bytecode.Function;
import dev.rusk.bytecode.HostImport;
import dev.rusk.bytecode.Rbc;
import dev.rusk.vm.AbiValue;
import dev.rusk.vm.ExternalEffectRegistry;
import dev.rusk.vm.HostImportRegistry;
import dev.rusk.vm.StepResult;
import dev.rusk.vm.Vm;
import dev.rusk.vm.VmError;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public final class RunRbcFromFile {
    private static final String STD_HOST_PRINT = "_std_host::print";
    private static final String STD_HOST_PRINTLN = "_std_host::println";

    public static void main(String[] args) throws Exception {
        Args parsed = Args.parse(args);
        if (parsed == null) {
            usage(System.err);
            System.exit(2);
            return;
        }
        if (parsed.help) {
            usage(System.out);
            return;
        }

        byte[] bytes = Files.readAllBytes(Path.of(parsed.rbcPath));
        ExecutableModule module = Rbc.fromBytes(bytes);

        if (parsed.dump) {
            dumpModule(module, System.out);
        }

        HostImportRegistry hostImports = new HostImportRegistry();
        installStdIo(hostImports);

        ExternalEffectRegistry effects = new ExternalEffectRegistry();
        installTestExternalEffects(effects);

        Vm vm = newVmWithAutoArgv(module, hostImports, parsed.programArgs);
        runToCompletion(vm, module, effects, System.out, System.err);
    }

    private static Vm newVmWithAutoArgv(ExecutableModule module, HostImportRegistry hostImports, List<String> argv)
            throws VmError {
        Function entryFn =
                module.function(module.entry())
                        .orElseThrow(() -> new VmError.InvalidState("invalid entry function id " + module.entry().index()));
        int paramCount = entryFn.paramCount();
        if (paramCount == 0) {
            return new Vm(module, hostImports);
        }
        if (paramCount == 1) {
            return new Vm(module, hostImports, argv);
        }
        throw new VmError.InvalidState("entry function param_count must be 0 or 1 (got " + paramCount + ")");
    }

    private static void runToCompletion(
            Vm vm,
            ExecutableModule module,
            ExternalEffectRegistry effects,
            PrintStream out,
            PrintStream err)
            throws Exception {
        while (true) {
            StepResult r = vm.step(null);
            if (r instanceof StepResult.Yield) {
                continue;
            }
            if (r instanceof StepResult.Done done) {
                printReturn(done.value(), out);
                return;
            }
            if (r instanceof StepResult.Trap trap) {
                err.println("runtime error: " + trap.message());
                System.exit(1);
                return;
            }
            if (r instanceof StepResult.Request req) {
                ExternalEffectDecl decl =
                        module.externalEffect(req.effectId())
                                .orElseThrow(
                                        () -> new IllegalStateException("unknown effect id " + req.effectId().index()));
                String name = decl.interfaceName() + "." + decl.method();

                var handler = effects.resolve(decl.interfaceName(), decl.method());
                if (handler == null) {
                    err.println("runtime error: unhandled external effect request: " + name + " args=" + req.args());
                    vm.dropContinuation(req.k());
                    System.exit(1);
                    return;
                }

                AbiValue ret;
                try {
                    ret = handler.handle(req.args());
                } catch (Exception e) {
                    vm.dropContinuation(req.k());
                    err.println("runtime error: external effect handler failed: " + name + ": " + e.getMessage());
                    System.exit(1);
                    return;
                }

                AbiType expected = decl.sig().ret();
                if (ret.ty() != expected) {
                    err.println(
                            "runtime error: external effect handler return type mismatch for "
                                    + name
                                    + ": expected "
                                    + expected
                                    + ", got "
                                    + ret.ty());
                    vm.dropContinuation(req.k());
                    System.exit(1);
                    return;
                }

                vm.resume(req.k(), ret);
                continue;
            }

            throw new IllegalStateException("unknown StepResult: " + r);
        }
    }

    private static void installStdIo(HostImportRegistry reg) {
        reg.registerByName(
                STD_HOST_PRINT,
                args -> {
                    if (args.size() != 1 || !(args.get(0) instanceof AbiValue.Str s)) {
                        throw new IllegalArgumentException(STD_HOST_PRINT + ": bad args: " + args);
                    }
                    System.out.print(s.value());
                    System.out.flush();
                    return new AbiValue.Unit();
                });

        reg.registerByName(
                STD_HOST_PRINTLN,
                args -> {
                    if (args.size() != 1 || !(args.get(0) instanceof AbiValue.Str s)) {
                        throw new IllegalArgumentException(STD_HOST_PRINTLN + ": bad args: " + args);
                    }
                    System.out.println(s.value());
                    System.out.flush();
                    return new AbiValue.Unit();
                });
    }

    private static void installTestExternalEffects(ExternalEffectRegistry reg) {
        reg.register(
                "TestFfi",
                "add",
                args -> {
                    if (args.size() != 2 || !(args.get(0) instanceof AbiValue.Int a) || !(args.get(1) instanceof AbiValue.Int b)) {
                        throw new IllegalArgumentException("TestFfi.add: bad args: " + args);
                    }
                    return new AbiValue.Int(a.value() + b.value());
                });

        reg.register(
                "TestFfi",
                "echo",
                args -> {
                    if (args.size() != 1 || !(args.get(0) instanceof AbiValue.Str s)) {
                        throw new IllegalArgumentException("TestFfi.echo: bad args: " + args);
                    }
                    return new AbiValue.Str(s.value());
                });

        reg.register(
                "TestFfi",
                "echo_bytes",
                args -> {
                    if (args.size() != 1 || !(args.get(0) instanceof AbiValue.Bytes b)) {
                        throw new IllegalArgumentException("TestFfi.echo_bytes: bad args: " + args);
                    }
                    return new AbiValue.Bytes(b.value());
                });
    }

    private static void printReturn(AbiValue v, PrintStream out) {
        if (v instanceof AbiValue.Unit) {
            return;
        }
        if (v instanceof AbiValue.Bool b) {
            out.println(b.value());
            return;
        }
        if (v instanceof AbiValue.Int n) {
            out.println(n.value());
            return;
        }
        if (v instanceof AbiValue.Float x) {
            out.println(x.value());
            return;
        }
        if (v instanceof AbiValue.Str s) {
            out.println(s.value());
            return;
        }
        if (v instanceof AbiValue.Bytes b) {
            out.println("bytes(" + b.value().length + "): " + Arrays.toString(b.value()));
            return;
        }
        out.println(v);
    }

    private static void dumpModule(ExecutableModule module, PrintStream out) {
        out.println("== module ==");
        out.println("functions: " + module.functions().size());
        out.println("host_imports: " + module.hostImports().size());
        for (int i = 0; i < module.hostImports().size(); i++) {
            HostImport imp = module.hostImports().get(i);
            out.println("  [" + i + "] " + imp.name() + " : " + imp.sig());
        }
        out.println("external_effects: " + module.externalEffects().size());
        for (int i = 0; i < module.externalEffects().size(); i++) {
            ExternalEffectDecl d = module.externalEffects().get(i);
            out.println("  [" + i + "] " + d.interfaceName() + "." + d.method() + " : " + d.sig());
        }
        out.println("entry: fn#" + module.entry().index());
    }

    private static void usage(PrintStream out) {
        out.println("usage: RunRbcFromFile [--dump] <file.rbc> [args...]");
    }

    private static final class Args {
        boolean help;
        boolean dump;
        String rbcPath;
        List<String> programArgs = List.of();

        static Args parse(String[] argv) {
            Args a = new Args();
            int i = 0;
            while (i < argv.length) {
                String s = argv[i];
                if (!s.startsWith("--")) {
                    break;
                }
                switch (s) {
                    case "--help", "-h" -> {
                        a.help = true;
                        i++;
                    }
                    case "--dump" -> {
                        a.dump = true;
                        i++;
                    }
                    case "--" -> {
                        i++;
                        break;
                    }
                    default -> {
                        return null;
                    }
                }
            }

            if (a.help) {
                return a;
            }

            if (i >= argv.length) {
                return null;
            }
            a.rbcPath = argv[i];
            i++;
            if (i < argv.length) {
                a.programArgs = List.copyOf(Arrays.asList(argv).subList(i, argv.length));
            }
            return a;
        }
    }
}

