EXPECTED = 200_001


def main() -> int:
    keep: list[tuple[int, int]] = [(0, 0)]
    i = 0
    while i < 200_000:
        keep.append((i, i + 1))
        i += 1
    grown = len(keep)

    cycle = 0
    while cycle < 100:
        j = 0
        while j < 20_000:
            _t = (j, j + 1)
            j += 1
        cycle += 1

    return grown

