package dev.rusk.bytecode;

public enum Intrinsic {
    StringConcat(0),
    ToString(1),
    Panic(2),

    BoolNot(3),
    BoolEq(4),
    BoolNe(5),

    IntAdd(6),
    IntSub(7),
    IntMul(8),
    IntDiv(9),
    IntMod(10),
    IntAnd(72),
    IntOr(73),
    IntXor(74),
    IntNot(75),
    IntShl(76),
    IntShr(77),
    IntUShr(78),
    IntEq(11),
    IntNe(12),
    IntLt(13),
    IntLe(14),
    IntGt(15),
    IntGe(16),

    FloatAdd(17),
    FloatSub(18),
    FloatMul(19),
    FloatDiv(20),
    FloatMod(21),
    FloatEq(22),
    FloatNe(23),
    FloatLt(24),
    FloatLe(25),
    FloatGt(26),
    FloatGe(27),

    StringEq(28),
    StringNe(29),
    BytesEq(30),
    BytesNe(31),
    UnitEq(32),
    UnitNe(33),

    ArrayLen(34),
    ArrayLenRo(35),
    ArrayPush(36),
    ArrayPop(37),
    ArrayClear(38),
    ArrayResize(39),
    ArrayInsert(40),
    ArrayRemove(41),
    ArrayExtend(42),
    ArrayConcat(43),
    ArrayConcatRo(44),
    ArraySlice(45),
    ArraySliceRo(46),

    IntToByte(47),
    IntTryByte(48),
    ByteToInt(49),
    ByteAnd(79),
    ByteOr(80),
    ByteXor(81),
    ByteNot(82),
    ByteShl(83),
    ByteShr(84),
    ByteUShr(85),
    IntToChar(50),
    IntTryChar(51),
    CharToInt(52),

    BytesGet(53),
    BytesLen(54),
    BytesSlice(55),
    BytesToArray(56),
    BytesFromArray(57),

    StringSlice(58),
    StringNextIndex(59),
    StringCodepointAt(60),
    StringFromChars(61),
    StringFromUtf8(62),
    StringFromUtf8Strict(63),
    StringFromUtf16Le(64),
    StringFromUtf16LeStrict(65),
    StringFromUtf16Be(66),
    StringFromUtf16BeStrict(67),

    HashInt(68),
    HashString(69),
    HashBytes(70),
    HashCombine(71);

    private final int tag;

    Intrinsic(int tag) {
        this.tag = tag;
    }

    public int tag() {
        return tag;
    }

    public static Intrinsic fromTag(int tag) {
        return switch (tag) {
            case 0 -> StringConcat;
            case 1 -> ToString;
            case 2 -> Panic;
            case 3 -> BoolNot;
            case 4 -> BoolEq;
            case 5 -> BoolNe;
            case 6 -> IntAdd;
            case 7 -> IntSub;
            case 8 -> IntMul;
            case 9 -> IntDiv;
            case 10 -> IntMod;
            case 72 -> IntAnd;
            case 73 -> IntOr;
            case 74 -> IntXor;
            case 75 -> IntNot;
            case 76 -> IntShl;
            case 77 -> IntShr;
            case 78 -> IntUShr;
            case 11 -> IntEq;
            case 12 -> IntNe;
            case 13 -> IntLt;
            case 14 -> IntLe;
            case 15 -> IntGt;
            case 16 -> IntGe;
            case 17 -> FloatAdd;
            case 18 -> FloatSub;
            case 19 -> FloatMul;
            case 20 -> FloatDiv;
            case 21 -> FloatMod;
            case 22 -> FloatEq;
            case 23 -> FloatNe;
            case 24 -> FloatLt;
            case 25 -> FloatLe;
            case 26 -> FloatGt;
            case 27 -> FloatGe;
            case 28 -> StringEq;
            case 29 -> StringNe;
            case 30 -> BytesEq;
            case 31 -> BytesNe;
            case 32 -> UnitEq;
            case 33 -> UnitNe;
            case 34 -> ArrayLen;
            case 35 -> ArrayLenRo;
            case 36 -> ArrayPush;
            case 37 -> ArrayPop;
            case 38 -> ArrayClear;
            case 39 -> ArrayResize;
            case 40 -> ArrayInsert;
            case 41 -> ArrayRemove;
            case 42 -> ArrayExtend;
            case 43 -> ArrayConcat;
            case 44 -> ArrayConcatRo;
            case 45 -> ArraySlice;
            case 46 -> ArraySliceRo;
            case 47 -> IntToByte;
            case 48 -> IntTryByte;
            case 49 -> ByteToInt;
            case 79 -> ByteAnd;
            case 80 -> ByteOr;
            case 81 -> ByteXor;
            case 82 -> ByteNot;
            case 83 -> ByteShl;
            case 84 -> ByteShr;
            case 85 -> ByteUShr;
            case 50 -> IntToChar;
            case 51 -> IntTryChar;
            case 52 -> CharToInt;
            case 53 -> BytesGet;
            case 54 -> BytesLen;
            case 55 -> BytesSlice;
            case 56 -> BytesToArray;
            case 57 -> BytesFromArray;
            case 58 -> StringSlice;
            case 59 -> StringNextIndex;
            case 60 -> StringCodepointAt;
            case 61 -> StringFromChars;
            case 62 -> StringFromUtf8;
            case 63 -> StringFromUtf8Strict;
            case 64 -> StringFromUtf16Le;
            case 65 -> StringFromUtf16LeStrict;
            case 66 -> StringFromUtf16Be;
            case 67 -> StringFromUtf16BeStrict;
            case 68 -> HashInt;
            case 69 -> HashString;
            case 70 -> HashBytes;
            case 71 -> HashCombine;
            default -> throw new IllegalArgumentException("invalid Intrinsic tag " + tag);
        };
    }
}
