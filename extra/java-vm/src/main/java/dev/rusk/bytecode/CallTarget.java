package dev.rusk.bytecode;

import java.util.Objects;

public sealed interface CallTarget permits CallTarget.Bc, CallTarget.Host, CallTarget.IntrinsicTarget {
    record Bc(FunctionId function) implements CallTarget {
        public Bc {
            Objects.requireNonNull(function, "function");
        }
    }

    record Host(HostImportId hostImport) implements CallTarget {
        public Host {
            Objects.requireNonNull(hostImport, "hostImport");
        }
    }

    record IntrinsicTarget(Intrinsic intrinsic) implements CallTarget {
        public IntrinsicTarget {
            Objects.requireNonNull(intrinsic, "intrinsic");
        }
    }
}

