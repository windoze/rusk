EXPECTED = 300_040_000


def main() -> int:
    n = 20_000
    d: dict[int, int] = {}

    # Insert.
    i = 0
    while i < n:
        d[i] = i + 1
        i += 1

    # Lookup sum.
    sum1 = 0
    j = 0
    while j < n:
        sum1 += d[j]
        j += 1

    # Update (overwrite existing keys).
    k = 0
    while k < n:
        d[k] = k + 2
        k += 1

    # Remove every other key (even keys).
    r = 0
    while r < n:
        if r % 2 == 0:
            d.pop(r, None)
        r += 1

    # Lookup sum after removals.
    sum2 = 0
    t = 0
    while t < n:
        sum2 += d.get(t, 0)
        t += 1

    return sum1 + sum2 + len(d)

