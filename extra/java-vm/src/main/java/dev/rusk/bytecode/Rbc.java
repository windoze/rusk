package dev.rusk.bytecode;

public final class Rbc {
    private Rbc() {}

    public static byte[] toBytes(ExecutableModule module) throws RbcEncodeException {
        return new RbcEncoder().encode(module);
    }

    public static ExecutableModule fromBytes(byte[] bytes) throws RbcDecodeException {
        return new RbcDecoder().decode(bytes);
    }
}

