package dev.rusk.vm;

import dev.rusk.bytecode.FunctionId;
import dev.rusk.bytecode.Pattern;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

sealed interface ReturnDsts permits ReturnDsts.None, ReturnDsts.One, ReturnDsts.Multi {
    static ReturnDsts fromOption(Integer dst) {
        if (dst == null) {
            return new None();
        }
        return new One(dst);
    }

    record None() implements ReturnDsts {}

    record One(int dst) implements ReturnDsts {}

    record Multi(List<Integer> dsts) implements ReturnDsts {
        public Multi {
            Objects.requireNonNull(dsts, "dsts");
            dsts = List.copyOf(dsts);
        }
    }
}

final class Frame {
    final FunctionId func;
    int pc;
    final Value[] regs; // null 表示 uninitialized
    ReturnDsts returnDsts;

    Frame(FunctionId func, int pc, Value[] regs, ReturnDsts returnDsts) {
        this.func = Objects.requireNonNull(func, "func");
        this.pc = pc;
        this.regs = Objects.requireNonNull(regs, "regs");
        this.returnDsts = Objects.requireNonNull(returnDsts, "returnDsts");
    }

    Frame deepCopy() {
        Value[] copied = new Value[regs.length];
        System.arraycopy(regs, 0, copied, 0, regs.length);
        return new Frame(func, pc, copied, returnDsts);
    }
}

final class RuntimeEffectId {
    final String interfaceName;
    final List<TypeReps.TypeRepId> interfaceArgs;
    final String method;

    RuntimeEffectId(String interfaceName, List<TypeReps.TypeRepId> interfaceArgs, String method) {
        this.interfaceName = Objects.requireNonNull(interfaceName, "interfaceName");
        this.interfaceArgs = List.copyOf(Objects.requireNonNull(interfaceArgs, "interfaceArgs"));
        this.method = Objects.requireNonNull(method, "method");
    }
}

final class RuntimeHandlerClause {
    final RuntimeEffectId effect;
    final List<Pattern> argPatterns;
    final int targetPc;
    final List<Integer> paramRegs;

    RuntimeHandlerClause(RuntimeEffectId effect, List<Pattern> argPatterns, int targetPc, List<Integer> paramRegs) {
        this.effect = Objects.requireNonNull(effect, "effect");
        this.argPatterns = List.copyOf(Objects.requireNonNull(argPatterns, "argPatterns"));
        this.targetPc = targetPc;
        this.paramRegs = List.copyOf(Objects.requireNonNull(paramRegs, "paramRegs"));
    }
}

final class HandlerEntry {
    int ownerDepth;
    final List<RuntimeHandlerClause> clauses;

    HandlerEntry(int ownerDepth, List<RuntimeHandlerClause> clauses) {
        this.ownerDepth = ownerDepth;
        this.clauses = List.copyOf(Objects.requireNonNull(clauses, "clauses"));
    }

    HandlerEntry shallowCopy() {
        return new HandlerEntry(ownerDepth, clauses);
    }

    static List<HandlerEntry> deepCopyList(List<HandlerEntry> handlers) {
        ArrayList<HandlerEntry> out = new ArrayList<>(handlers.size());
        for (HandlerEntry h : handlers) {
            out.add(h.shallowCopy());
        }
        return out;
    }
}

