package dev.rusk.bytecode;

import java.util.List;
import java.util.Objects;

/**
 * performed effect identity: {@code interface + instantiated interface args + method}.
 *
 * <p>这里的 {@code interfaceArgs} 存的是运行时 typerep 的寄存器编号（Reg）。</p>
 */
public record EffectSpec(java.lang.String interfaceName, List<Integer> interfaceArgs, java.lang.String method) {
    public EffectSpec {
        Objects.requireNonNull(interfaceName, "interfaceName");
        Objects.requireNonNull(interfaceArgs, "interfaceArgs");
        Objects.requireNonNull(method, "method");
        interfaceArgs = List.copyOf(interfaceArgs);
    }
}

