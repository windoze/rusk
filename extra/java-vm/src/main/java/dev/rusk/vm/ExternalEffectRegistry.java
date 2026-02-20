package dev.rusk.vm;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public final class ExternalEffectRegistry {
    private final Map<Key, ExternalEffectHandler> handlers = new HashMap<>();

    public void register(String interfaceName, String method, ExternalEffectHandler handler) {
        Objects.requireNonNull(interfaceName, "interfaceName");
        Objects.requireNonNull(method, "method");
        Objects.requireNonNull(handler, "handler");
        handlers.put(new Key(interfaceName, method), handler);
    }

    public ExternalEffectHandler resolve(String interfaceName, String method) {
        Objects.requireNonNull(interfaceName, "interfaceName");
        Objects.requireNonNull(method, "method");
        return handlers.get(new Key(interfaceName, method));
    }

    private record Key(String interfaceName, String method) {
        private Key {
            Objects.requireNonNull(interfaceName, "interfaceName");
            Objects.requireNonNull(method, "method");
        }
    }
}

