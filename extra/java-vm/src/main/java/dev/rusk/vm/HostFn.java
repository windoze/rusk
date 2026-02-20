package dev.rusk.vm;

import java.util.List;

@FunctionalInterface
public interface HostFn {
    AbiValue call(List<AbiValue> args) throws Exception;
}

