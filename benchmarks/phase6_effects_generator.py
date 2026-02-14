EXPECTED = 49_995_000


def range_gen(end: int):
    i = 0
    while i < end:
        yield i
        i += 1


def main() -> int:
    total = 0
    for v in range_gen(10_000):
        total += v
    return total
