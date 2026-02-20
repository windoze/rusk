package dev.rusk.vm;

import java.util.List;

@FunctionalInterface
public interface ExternalEffectHandler {
    AbiValue handle(List<AbiValue> args) throws Exception;
}

