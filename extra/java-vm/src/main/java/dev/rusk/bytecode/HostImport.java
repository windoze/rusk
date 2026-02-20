package dev.rusk.bytecode;

import java.util.Objects;

public record HostImport(String name, HostFnSig sig) {
    public HostImport {
        Objects.requireNonNull(name, "name");
        Objects.requireNonNull(sig, "sig");
        if (name.isEmpty()) {
            throw new IllegalArgumentException("host import name must be non-empty");
        }
    }
}

