package dev.rusk.bytecode;

import java.util.List;
import java.util.Objects;

public record HostFnSig(List<AbiType> params, AbiType ret) {
    public HostFnSig {
        Objects.requireNonNull(params, "params");
        Objects.requireNonNull(ret, "ret");
        params = List.copyOf(params);
    }
}

