package dev.rusk.bytecode;

import java.util.List;
import java.util.Objects;

public sealed interface Instruction
        permits Instruction.Const,
                Instruction.Copy,
                Instruction.Move,
                Instruction.AsReadonly,
                Instruction.IsType,
                Instruction.MakeTypeRep,
                Instruction.MakeStruct,
                Instruction.MakeArray,
                Instruction.MakeTuple,
                Instruction.MakeEnum,
                Instruction.GetField,
                Instruction.SetField,
                Instruction.StructGet,
                Instruction.StructSet,
                Instruction.TupleGet,
                Instruction.TupleSet,
                Instruction.IndexGet,
                Instruction.IndexSet,
                Instruction.Len,
                Instruction.IntAdd,
                Instruction.IntSub,
                Instruction.IntMul,
                Instruction.IntDiv,
                Instruction.IntMod,
                Instruction.IntAnd,
                Instruction.IntOr,
                Instruction.IntXor,
                Instruction.IntShl,
                Instruction.IntShr,
                Instruction.IntUShr,
                Instruction.IntNot,
                Instruction.IntLt,
                Instruction.IntLe,
                Instruction.IntGt,
                Instruction.IntGe,
                Instruction.IntEq,
                Instruction.IntNe,
                Instruction.ByteAnd,
                Instruction.ByteOr,
                Instruction.ByteXor,
                Instruction.ByteShl,
                Instruction.ByteShr,
                Instruction.ByteUShr,
                Instruction.ByteNot,
                Instruction.BoolNot,
                Instruction.BoolEq,
                Instruction.BoolNe,
                Instruction.Call,
                Instruction.CallMulti,
                Instruction.ICall,
                Instruction.VCall,
                Instruction.PushHandler,
                Instruction.PopHandler,
                Instruction.Perform,
                Instruction.Resume,
                Instruction.ResumeTail,
                Instruction.Jump,
                Instruction.JumpIf,
                Instruction.Switch,
                Instruction.Return,
                Instruction.ReturnMulti,
                Instruction.Trap {
    record Const(int dst, ConstValue value) implements Instruction {
        public Const {
            Objects.requireNonNull(value, "value");
        }
    }

    record Copy(int dst, int src) implements Instruction {}

    record Move(int dst, int src) implements Instruction {}

    record AsReadonly(int dst, int src) implements Instruction {}

    record IsType(int dst, int value, int ty) implements Instruction {}

    record MakeTypeRep(int dst, TypeRepLit base, List<Integer> args) implements Instruction {
        public MakeTypeRep {
            Objects.requireNonNull(base, "base");
            Objects.requireNonNull(args, "args");
            args = List.copyOf(args);
        }
    }

    record MakeStruct(
            int dst,
            TypeId typeId,
            List<Integer> typeArgs,
            List<FieldInit> fields)
            implements Instruction {
        public MakeStruct {
            Objects.requireNonNull(typeId, "typeId");
            Objects.requireNonNull(typeArgs, "typeArgs");
            Objects.requireNonNull(fields, "fields");
            typeArgs = List.copyOf(typeArgs);
            fields = List.copyOf(fields);
        }

        public record FieldInit(java.lang.String name, int reg) {
            public FieldInit {
                Objects.requireNonNull(name, "name");
            }
        }
    }

    record MakeArray(int dst, List<Integer> items) implements Instruction {
        public MakeArray {
            Objects.requireNonNull(items, "items");
            items = List.copyOf(items);
        }
    }

    record MakeTuple(int dst, List<Integer> items) implements Instruction {
        public MakeTuple {
            Objects.requireNonNull(items, "items");
            items = List.copyOf(items);
        }
    }

    record MakeEnum(
            int dst,
            TypeId enumTypeId,
            List<Integer> typeArgs,
            java.lang.String variant,
            List<Integer> fields)
            implements Instruction {
        public MakeEnum {
            Objects.requireNonNull(enumTypeId, "enumTypeId");
            Objects.requireNonNull(typeArgs, "typeArgs");
            Objects.requireNonNull(variant, "variant");
            Objects.requireNonNull(fields, "fields");
            typeArgs = List.copyOf(typeArgs);
            fields = List.copyOf(fields);
        }
    }

    record GetField(int dst, int obj, java.lang.String field) implements Instruction {
        public GetField {
            Objects.requireNonNull(field, "field");
        }
    }

    record SetField(int obj, java.lang.String field, int value) implements Instruction {
        public SetField {
            Objects.requireNonNull(field, "field");
        }
    }

    record StructGet(int dst, int obj, int idx) implements Instruction {}

    record StructSet(int obj, int idx, int value) implements Instruction {}

    record TupleGet(int dst, int tup, int idx) implements Instruction {}

    record TupleSet(int tup, int idx, int value) implements Instruction {}

    record IndexGet(int dst, int arr, int idx) implements Instruction {}

    record IndexSet(int arr, int idx, int value) implements Instruction {}

    record Len(int dst, int arr) implements Instruction {}

    record IntAdd(int dst, int a, int b) implements Instruction {}

    record IntSub(int dst, int a, int b) implements Instruction {}

    record IntMul(int dst, int a, int b) implements Instruction {}

    record IntDiv(int dst, int a, int b) implements Instruction {}

    record IntMod(int dst, int a, int b) implements Instruction {}

    record IntAnd(int dst, int a, int b) implements Instruction {}

    record IntOr(int dst, int a, int b) implements Instruction {}

    record IntXor(int dst, int a, int b) implements Instruction {}

    record IntShl(int dst, int a, int b) implements Instruction {}

    record IntShr(int dst, int a, int b) implements Instruction {}

    record IntUShr(int dst, int a, int b) implements Instruction {}

    record IntNot(int dst, int v) implements Instruction {}

    record IntLt(int dst, int a, int b) implements Instruction {}

    record IntLe(int dst, int a, int b) implements Instruction {}

    record IntGt(int dst, int a, int b) implements Instruction {}

    record IntGe(int dst, int a, int b) implements Instruction {}

    record IntEq(int dst, int a, int b) implements Instruction {}

    record IntNe(int dst, int a, int b) implements Instruction {}

    record ByteAnd(int dst, int a, int b) implements Instruction {}

    record ByteOr(int dst, int a, int b) implements Instruction {}

    record ByteXor(int dst, int a, int b) implements Instruction {}

    record ByteShl(int dst, int a, int b) implements Instruction {}

    record ByteShr(int dst, int a, int b) implements Instruction {}

    record ByteUShr(int dst, int a, int b) implements Instruction {}

    record ByteNot(int dst, int v) implements Instruction {}

    record BoolNot(int dst, int v) implements Instruction {}

    record BoolEq(int dst, int a, int b) implements Instruction {}

    record BoolNe(int dst, int a, int b) implements Instruction {}

    record Call(Integer dst, CallTarget func, List<Integer> args) implements Instruction {
        public Call {
            Objects.requireNonNull(func, "func");
            Objects.requireNonNull(args, "args");
            args = List.copyOf(args);
        }
    }

    record CallMulti(List<Integer> dsts, CallTarget func, List<Integer> args) implements Instruction {
        public CallMulti {
            Objects.requireNonNull(dsts, "dsts");
            Objects.requireNonNull(func, "func");
            Objects.requireNonNull(args, "args");
            dsts = List.copyOf(dsts);
            args = List.copyOf(args);
        }
    }

    record ICall(Integer dst, int fnptr, List<Integer> args) implements Instruction {
        public ICall {
            Objects.requireNonNull(args, "args");
            args = List.copyOf(args);
        }
    }

    record VCall(
            Integer dst,
            int obj,
            MethodId method,
            List<Integer> methodTypeArgs,
            List<Integer> args)
            implements Instruction {
        public VCall {
            Objects.requireNonNull(method, "method");
            Objects.requireNonNull(methodTypeArgs, "methodTypeArgs");
            Objects.requireNonNull(args, "args");
            methodTypeArgs = List.copyOf(methodTypeArgs);
            args = List.copyOf(args);
        }
    }

    record PushHandler(List<HandlerClause> clauses) implements Instruction {
        public PushHandler {
            Objects.requireNonNull(clauses, "clauses");
            clauses = List.copyOf(clauses);
        }
    }

    record PopHandler() implements Instruction {}

    record Perform(Integer dst, EffectSpec effect, List<Integer> args) implements Instruction {
        public Perform {
            Objects.requireNonNull(effect, "effect");
            Objects.requireNonNull(args, "args");
            args = List.copyOf(args);
        }
    }

    record Resume(Integer dst, int k, int value) implements Instruction {}

    record ResumeTail(int k, int value) implements Instruction {}

    record Jump(int targetPc) implements Instruction {}

    record JumpIf(int cond, int thenPc, int elsePc) implements Instruction {}

    record Switch(int value, List<SwitchCase> cases, int defaultPc) implements Instruction {
        public Switch {
            Objects.requireNonNull(cases, "cases");
            cases = List.copyOf(cases);
        }
    }

    record Return(int value) implements Instruction {}

    record ReturnMulti(List<Integer> values) implements Instruction {
        public ReturnMulti {
            Objects.requireNonNull(values, "values");
            values = List.copyOf(values);
        }
    }

    record Trap(java.lang.String message) implements Instruction {
        public Trap {
            Objects.requireNonNull(message, "message");
        }
    }
}
