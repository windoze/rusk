package dev.rusk.vm;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;

/**
 * Rusk 运行时的不可变 UTF-8 string（以“视图（start,len）”方式表示，索引按字节计）。
 */
final class RuskString {
    private final byte[] buf;
    private final int start;
    private final int len;

    RuskString(String s) {
        Objects.requireNonNull(s, "s");
        byte[] bytes = s.getBytes(StandardCharsets.UTF_8);
        this.buf = bytes;
        this.start = 0;
        this.len = bytes.length;
    }

    static RuskString fromUtf8BytesStrict(byte[] bytes) throws CharacterCodingException {
        Objects.requireNonNull(bytes, "bytes");
        CharsetDecoder dec =
                StandardCharsets.UTF_8
                        .newDecoder()
                        .onMalformedInput(CodingErrorAction.REPORT)
                        .onUnmappableCharacter(CodingErrorAction.REPORT);
        dec.decode(ByteBuffer.wrap(bytes));
        return new RuskString(Arrays.copyOf(bytes, bytes.length), 0, bytes.length);
    }

    static Optional<RuskString> tryFromUtf8BytesStrict(byte[] bytes) {
        try {
            return Optional.of(fromUtf8BytesStrict(bytes));
        } catch (CharacterCodingException e) {
            return Optional.empty();
        }
    }

    static RuskString fromUtf8BytesLossy(byte[] bytes) {
        Objects.requireNonNull(bytes, "bytes");
        String s = new String(bytes, StandardCharsets.UTF_8);
        return new RuskString(s);
    }

    private RuskString(byte[] buf, int start, int len) {
        this.buf = buf;
        this.start = start;
        this.len = len;
    }

    int byteLen() {
        return len;
    }

    byte[] toUtf8Bytes() {
        return Arrays.copyOfRange(buf, start, start + len);
    }

    String asJavaString() {
        return new String(buf, start, len, StandardCharsets.UTF_8);
    }

    boolean isCharBoundary(int byteIndex) {
        if (byteIndex < 0 || byteIndex > len) {
            return false;
        }
        if (byteIndex == 0 || byteIndex == len) {
            return true;
        }
        // UTF-8 continuation bytes have prefix 10xxxxxx.
        int b = buf[start + byteIndex] & 0xFF;
        return (b & 0b1100_0000) != 0b1000_0000;
    }

    record NextChar(int codePoint, int nextByteIndex) {}

    NextChar decodeNextChar(int byteIndex) throws CharacterCodingException {
        if (byteIndex < 0 || byteIndex > len) {
            throw new IndexOutOfBoundsException("byteIndex=" + byteIndex + ", len=" + len);
        }
        if (byteIndex == len) {
            throw new CharacterCodingException();
        }
        if (!isCharBoundary(byteIndex)) {
            throw new CharacterCodingException();
        }
        // 手写一次 UTF-8 解码（仅解一个 code point），避免全量 decode。
        int i = start + byteIndex;
        int b0 = buf[i] & 0xFF;
        if (b0 < 0x80) {
            return new NextChar(b0, byteIndex + 1);
        }
        if ((b0 & 0b1110_0000) == 0b1100_0000) {
            if (byteIndex + 2 > len) {
                throw new CharacterCodingException();
            }
            int b1 = buf[i + 1] & 0xFF;
            if ((b1 & 0b1100_0000) != 0b1000_0000) {
                throw new CharacterCodingException();
            }
            int cp = ((b0 & 0b0001_1111) << 6) | (b1 & 0b0011_1111);
            if (cp < 0x80) {
                throw new CharacterCodingException();
            }
            return new NextChar(cp, byteIndex + 2);
        }
        if ((b0 & 0b1111_0000) == 0b1110_0000) {
            if (byteIndex + 3 > len) {
                throw new CharacterCodingException();
            }
            int b1 = buf[i + 1] & 0xFF;
            int b2 = buf[i + 2] & 0xFF;
            if ((b1 & 0b1100_0000) != 0b1000_0000 || (b2 & 0b1100_0000) != 0b1000_0000) {
                throw new CharacterCodingException();
            }
            int cp = ((b0 & 0b0000_1111) << 12) | ((b1 & 0b0011_1111) << 6) | (b2 & 0b0011_1111);
            if (cp < 0x800 || (0xD800 <= cp && cp <= 0xDFFF)) {
                throw new CharacterCodingException();
            }
            return new NextChar(cp, byteIndex + 3);
        }
        if ((b0 & 0b1111_1000) == 0b1111_0000) {
            if (byteIndex + 4 > len) {
                throw new CharacterCodingException();
            }
            int b1 = buf[i + 1] & 0xFF;
            int b2 = buf[i + 2] & 0xFF;
            int b3 = buf[i + 3] & 0xFF;
            if ((b1 & 0b1100_0000) != 0b1000_0000
                    || (b2 & 0b1100_0000) != 0b1000_0000
                    || (b3 & 0b1100_0000) != 0b1000_0000) {
                throw new CharacterCodingException();
            }
            int cp =
                    ((b0 & 0b0000_0111) << 18)
                            | ((b1 & 0b0011_1111) << 12)
                            | ((b2 & 0b0011_1111) << 6)
                            | (b3 & 0b0011_1111);
            if (cp < 0x10000 || cp > 0x10FFFF) {
                throw new CharacterCodingException();
            }
            return new NextChar(cp, byteIndex + 4);
        }
        throw new CharacterCodingException();
    }

    RuskString slice(int from, int toExclusive) throws CharacterCodingException {
        if (from < 0 || toExclusive < 0) {
            throw new CharacterCodingException();
        }
        if (from > toExclusive) {
            throw new CharacterCodingException();
        }
        if (toExclusive > len) {
            throw new CharacterCodingException();
        }
        if (!isCharBoundary(from) || !isCharBoundary(toExclusive)) {
            throw new CharacterCodingException();
        }
        return new RuskString(buf, start + from, toExclusive - from);
    }

    RuskString concat(RuskString other) {
        int newLen = this.len + other.len;
        byte[] out = new byte[newLen];
        System.arraycopy(this.buf, this.start, out, 0, this.len);
        System.arraycopy(other.buf, other.start, out, this.len, other.len);
        return new RuskString(out, 0, out.length);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof RuskString other)) {
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

