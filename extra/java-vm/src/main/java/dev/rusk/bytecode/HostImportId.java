package dev.rusk.bytecode;

public record HostImportId(int index) {
    public HostImportId {
        if (index < 0) {
            throw new IllegalArgumentException("HostImportId must be non-negative");
        }
    }
}

