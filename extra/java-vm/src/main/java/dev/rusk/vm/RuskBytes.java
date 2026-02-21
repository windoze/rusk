package dev.rusk.vm;

import java.util.Arrays;
import java.util.Objects;

/**
 * Rusk 运行时的不可变 bytes 值（UTF-8 无关），以“视图（start,len）”方式避免频繁复制。
 */
final class RuskBytes {
    private final byte[] buf;
    private final int start;
    private final int len;

    RuskBytes(byte[] bytes) {
        Objects.requireNonNull(bytes, "bytes");
        this.buf = Arrays.copyOf(bytes, bytes.length);
        this.start = 0;
        this.len = bytes.length;
    }

    static RuskBytes view(byte[] buf, int start, int len) {
        Objects.requireNonNull(buf, "buf");
        if (start < 0 || len < 0) {
            throw new IndexOutOfBoundsException("start/len must be >= 0");
        }
        int end = start + len;
        if (end < start || end > buf.length) {
            throw new IndexOutOfBoundsException("range out of bounds: start=" + start + ", len=" + len);
        }
        return new RuskBytes(buf, start, len);
    }

    private RuskBytes(byte[] buf, int start, int len) {
        this.buf = buf;
        this.start = start;
        this.len = len;
    }

    int byteLen() {
        return len;
    }

    byte byteAt(int index) {
        if (index < 0 || index >= len) {
            throw new IndexOutOfBoundsException("index=" + index + ", len=" + len);
        }
        return buf[start + index];
    }

    RuskBytes slice(int from, int toExclusive) {
        if (from < 0 || toExclusive < 0) {
            throw new IllegalArgumentException("slice indices must be >= 0");
        }
        if (from > toExclusive) {
            throw new IllegalArgumentException("from > to");
        }
        if (toExclusive > len) {
            throw new IndexOutOfBoundsException("to out of bounds: to=" + toExclusive + ", len=" + len);
        }
        return new RuskBytes(buf, start + from, toExclusive - from);
    }

    byte[] toByteArray() {
        return Arrays.copyOfRange(buf, start, start + len);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof RuskBytes other)) {
            return false;
        }
        if (this.len != other.len) {
            return false;
        }
        for (int i = 0; i < len; i++) {
            if (this.buf[this.start + i] != other.buf[other.start + i]) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        int h = 1;
        for (int i = 0; i < len; i++) {
            h = 31 * h + buf[start + i];
        }
        return h;
    }
}
