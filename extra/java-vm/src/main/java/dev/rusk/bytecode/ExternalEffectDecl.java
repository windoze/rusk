package dev.rusk.bytecode;

import java.util.Objects;

public record ExternalEffectDecl(String interfaceName, String method, HostFnSig sig) {
    public ExternalEffectDecl {
        Objects.requireNonNull(interfaceName, "interfaceName");
        Objects.requireNonNull(method, "method");
        Objects.requireNonNull(sig, "sig");
        if (interfaceName.isEmpty()) {
            throw new IllegalArgumentException("external effect interface must be non-empty");
        }
        if (method.isEmpty()) {
            throw new IllegalArgumentException("external effect method must be non-empty");
        }
    }
}

