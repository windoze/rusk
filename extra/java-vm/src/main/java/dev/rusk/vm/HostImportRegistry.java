package dev.rusk.vm;

import dev.rusk.bytecode.HostImportId;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public final class HostImportRegistry {
    private final Map<Integer, HostFn> byId = new HashMap<>();
    private final Map<String, HostFn> byName = new HashMap<>();

    public void register(HostImportId id, HostFn fn) {
        Objects.requireNonNull(id, "id");
        Objects.requireNonNull(fn, "fn");
        byId.put(id.index(), fn);
    }

    public void registerByName(String name, HostFn fn) {
        Objects.requireNonNull(name, "name");
        Objects.requireNonNull(fn, "fn");
        byName.put(name, fn);
    }

    HostFn resolve(int id, String name) {
        HostFn fn = byId.get(id);
        if (fn != null) {
            return fn;
        }
        return byName.get(name);
    }
}

