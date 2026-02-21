package dev.rusk.bytecode;

public enum AbiType {
    UNIT(0),
    BOOL(1),
    INT(2),
    FLOAT(3),
    STRING(4),
    BYTES(5),
    CONTINUATION(6);

    private final int tag;

    AbiType(int tag) {
        this.tag = tag;
    }

    public int tag() {
        return tag;
    }

    public static AbiType fromTag(int tag) {
        return switch (tag) {
            case 0 -> UNIT;
            case 1 -> BOOL;
            case 2 -> INT;
            case 3 -> FLOAT;
            case 4 -> STRING;
            case 5 -> BYTES;
            case 6 -> CONTINUATION;
            default -> throw new IllegalArgumentException("invalid AbiType tag " + tag);
        };
    }
}
