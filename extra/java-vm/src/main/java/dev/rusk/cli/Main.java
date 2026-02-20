package dev.rusk.cli;

import dev.rusk.bytecode.AbiType;
import dev.rusk.bytecode.ExecutableModule;
import dev.rusk.bytecode.ExternalEffectDecl;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class Main {
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

        byte[] bytes;
        try {
            bytes = Files.readAllBytes(Path.of(parsed.rbcPath));
        } catch (Exception e) {
            System.err.println("failed to read file: " + e.getMessage());
            System.exit(1);
            return;
        }

        ExecutableModule module;
        try {
            module = Rbc.fromBytes(bytes);
        } catch (Exception e) {
            System.err.println("load error: " + e.getMessage());
            System.exit(1);
            return;
        }

        if (parsed.dump) {
            dumpModule(module, System.out);
        }

        HostImportRegistry hostImports = new HostImportRegistry();
        installStdIo(hostImports);
        if (parsed.allowTestHost) {
            installTestHost(hostImports);
        }

        ExternalEffectRegistry effects = new ExternalEffectRegistry();
        if (parsed.allowTestFfi) {
            installTestExternalEffects(effects);
        }

        Vm vm;
        try {
            if (parsed.programArgs.isEmpty()) {
                vm = new Vm(module, hostImports);
            } else {
                vm = new Vm(module, hostImports, parsed.programArgs);
            }
        } catch (VmError e) {
            System.err.println("vm init error: " + e.getMessage());
            if (!module.hostImports().isEmpty()) {
                System.err.println("declared host imports:");
                for (HostImport imp : module.hostImports()) {
                    System.err.println("  - " + imp.name() + " : " + imp.sig());
                }
            }
            System.exit(1);
            return;
        }

        runToCompletion(vm, module, effects, parsed.fuel);
    }

    private static void runToCompletion(
            Vm vm, ExecutableModule module, ExternalEffectRegistry effects, Long fuel) throws Exception {
        while (true) {
            StepResult r = vm.step(fuel);
            if (r instanceof StepResult.Yield) {
                continue;
            }
            if (r instanceof StepResult.Done done) {
                printReturn(done.value(), System.out);
                return;
            }
            if (r instanceof StepResult.Trap trap) {
                System.err.println("runtime error: " + trap.message());
                System.exit(1);
                return;
            }
            if (r instanceof StepResult.Request req) {
                ExternalEffectDecl decl =
                        module.externalEffect(req.effectId()).orElse(null);
                String name =
                        (decl == null)
                                ? "<unknown " + req.effectId().index() + ">"
                                : decl.interfaceName() + "." + decl.method();

                if (decl == null) {
                    System.err.println("runtime error: external effect request: " + name + " args=" + req.args());
                    System.exit(1);
                    return;
                }

                var handler = effects.resolve(decl.interfaceName(), decl.method());
                if (handler == null) {
                    System.err.println(
                            "runtime error: unhandled external effect request: " + name + " args=" + req.args());
                    System.err.println("hint: run with `--allow-test-ffi` for TestFfi.* handlers");
                    System.exit(1);
                    return;
                }

                AbiValue ret;
                try {
                    ret = handler.handle(req.args());
                } catch (Exception e) {
                    try {
                        vm.dropContinuation(req.k());
                    } catch (VmError ignored) {
                        // ignore
                    }
                    System.err.println("runtime error: external effect handler failed: " + name + ": " + e.getMessage());
                    System.exit(1);
                    return;
                }

                if (ret == null) {
                    System.err.println("runtime error: external effect handler returned null: " + name);
                    System.exit(1);
                    return;
                }

                AbiType expected = decl.sig().ret();
                if (ret.ty() != expected) {
                    System.err.println(
                            "runtime error: external effect handler return type mismatch for "
                                    + name
                                    + ": expected "
                                    + expected
                                    + ", got "
                                    + ret.ty());
                    System.exit(1);
                    return;
                }

                try {
                    vm.resume(req.k(), ret);
                } catch (VmError e) {
                    System.err.println("runtime error: resume failed: " + e.getMessage());
                    System.exit(1);
                    return;
                }
                continue;
            }

            System.err.println("runtime error: unknown StepResult: " + r);
            System.exit(1);
            return;
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

    private static void installTestHost(HostImportRegistry reg) {
        reg.registerByName(
                "test::add_int",
                args -> {
                    if (args.size() != 2 || !(args.get(0) instanceof AbiValue.Int a) || !(args.get(1) instanceof AbiValue.Int b)) {
                        throw new IllegalArgumentException("test::add_int: bad args: " + args);
                    }
                    return new AbiValue.Int(a.value() + b.value());
                });

        reg.registerByName(
                "test::concat_string",
                args -> {
                    if (args.size() != 2 || !(args.get(0) instanceof AbiValue.Str a) || !(args.get(1) instanceof AbiValue.Str b)) {
                        throw new IllegalArgumentException("test::concat_string: bad args: " + args);
                    }
                    return new AbiValue.Str(a.value() + b.value());
                });

        reg.registerByName(
                "test::bool_not",
                args -> {
                    if (args.size() != 1 || !(args.get(0) instanceof AbiValue.Bool v)) {
                        throw new IllegalArgumentException("test::bool_not: bad args: " + args);
                    }
                    return new AbiValue.Bool(!v.value());
                });

        reg.registerByName(
                "test::float_mul",
                args -> {
                    if (args.size() != 2 || !(args.get(0) instanceof AbiValue.Float a) || !(args.get(1) instanceof AbiValue.Float b)) {
                        throw new IllegalArgumentException("test::float_mul: bad args: " + args);
                    }
                    return new AbiValue.Float(a.value() * b.value());
                });

        reg.registerByName(
                "test::float_eq",
                args -> {
                    if (args.size() != 2 || !(args.get(0) instanceof AbiValue.Float a) || !(args.get(1) instanceof AbiValue.Float b)) {
                        throw new IllegalArgumentException("test::float_eq: bad args: " + args);
                    }
                    return new AbiValue.Bool(Double.compare(a.value(), b.value()) == 0);
                });

        reg.registerByName(
                "test::unit",
                args -> {
                    if (!args.isEmpty()) {
                        throw new IllegalArgumentException("test::unit: bad args: " + args);
                    }
                    return new AbiValue.Unit();
                });

        reg.registerByName(
                "test::bytes_echo",
                args -> {
                    if (args.size() != 1 || !(args.get(0) instanceof AbiValue.Bytes b)) {
                        throw new IllegalArgumentException("test::bytes_echo: bad args: " + args);
                    }
                    return new AbiValue.Bytes(b.value());
                });

        reg.registerByName(
                "test::bytes_eq",
                args -> {
                    if (args.size() != 2 || !(args.get(0) instanceof AbiValue.Bytes a) || !(args.get(1) instanceof AbiValue.Bytes b)) {
                        throw new IllegalArgumentException("test::bytes_eq: bad args: " + args);
                    }
                    return new AbiValue.Bool(Arrays.equals(a.value(), b.value()));
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
        boolean anyGeneric = module.functionGenericParams().stream().anyMatch(n -> n != 0);
        if (anyGeneric) {
            out.println("function_generic_params:");
            for (int i = 0; i < module.functionGenericParams().size(); i++) {
                int n = module.functionGenericParams().get(i);
                if (n != 0) {
                    out.println("  fn#" + i + " : " + n);
                }
            }
        }
        out.println("host_imports: " + module.hostImports().size());
        for (int i = 0; i < module.hostImports().size(); i++) {
            HostImport imp = module.hostImports().get(i);
            out.println("  [" + i + "] " + imp.name() + " : " + imp.sig());
        }
        out.println("types: " + module.typeNames().size());
        for (int i = 0; i < module.typeNames().size(); i++) {
            out.println("  [" + i + "] " + module.typeNames().get(i));
        }
        out.println("methods: " + module.methodNames().size());
        for (int i = 0; i < module.methodNames().size(); i++) {
            out.println("  [" + i + "] " + module.methodNames().get(i));
        }
        out.println("vcall_dispatch:");
        for (int typeId = 0; typeId < module.vcallDispatch().size(); typeId++) {
            var entries = module.vcallDispatch().get(typeId);
            if (entries.isEmpty()) {
                continue;
            }
            String typeName = module.typeNames().get(typeId);
            out.println("  type[" + typeId + "] " + typeName);
            for (var entry : entries) {
                String methodName =
                        module.methodName(entry.method()).orElse("#" + entry.method().index());
                out.println("    " + methodName + " -> fn#" + entry.function().index());
            }
        }
        out.println("interface_impls:");
        for (int typeId = 0; typeId < module.interfaceImpls().size(); typeId++) {
            var ifaces = module.interfaceImpls().get(typeId);
            if (ifaces.isEmpty()) {
                continue;
            }
            out.print("  type[" + typeId + "] " + module.typeNames().get(typeId) + " : ");
            for (int i = 0; i < ifaces.size(); i++) {
                var ifaceId = ifaces.get(i);
                String ifaceName = module.typeName(ifaceId).orElse("#" + ifaceId.index());
                if (i != 0) {
                    out.print(", ");
                }
                out.print(ifaceName);
            }
            out.println();
        }
        out.println("struct_layouts:");
        for (int typeId = 0; typeId < module.structLayouts().size(); typeId++) {
            var layout = module.structLayouts().get(typeId);
            if (layout == null) {
                continue;
            }
            out.println("  type[" + typeId + "] " + module.typeNames().get(typeId) + " : " + layout);
        }
        out.println("external_effects: " + module.externalEffects().size());
        for (int i = 0; i < module.externalEffects().size(); i++) {
            ExternalEffectDecl d = module.externalEffects().get(i);
            out.println("  [" + i + "] " + d.interfaceName() + "." + d.method() + " : " + d.sig());
        }
        out.println("entry: fn#" + module.entry().index());
    }

    private static void usage(PrintStream out) {
        out.println("usage: rusk-java-vm [--fuel N] [--dump] [--allow-test-host] [--allow-test-ffi] <file.rbc> [args...]");
    }

    private static final class Args {
        boolean help;
        boolean dump;
        boolean allowTestHost;
        boolean allowTestFfi;
        Long fuel;
        String rbcPath;
        List<String> programArgs = List.of();

        static Args parse(String[] argv) {
            Args a = new Args();
            ArrayList<String> args = new ArrayList<>(List.of(argv));
            int i = 0;
            while (i < args.size()) {
                String s = args.get(i);
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
                    case "--allow-test-host" -> {
                        a.allowTestHost = true;
                        i++;
                    }
                    case "--allow-test-ffi" -> {
                        a.allowTestFfi = true;
                        i++;
                    }
                    case "--fuel" -> {
                        if (i + 1 >= args.size()) {
                            return null;
                        }
                        try {
                            a.fuel = Long.parseLong(args.get(i + 1));
                        } catch (NumberFormatException e) {
                            return null;
                        }
                        i += 2;
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

            if (i >= args.size()) {
                return null;
            }

            a.rbcPath = args.get(i);
            i++;
            if (i < args.size()) {
                a.programArgs = List.copyOf(args.subList(i, args.size()));
            }
            return a;
        }
    }
}
