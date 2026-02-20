package dev.rusk.bytecode;

public final class RbcDecodeException extends Exception {
    private final int offset;

    public RbcDecodeException(String message, int offset) {
        super("decode error at " + offset + ": " + message);
        this.offset = offset;
    }

    public int offset() {
        return offset;
    }
}

