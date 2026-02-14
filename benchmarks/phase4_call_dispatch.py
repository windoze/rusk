EXPECTED = 500_000


def inc(x: int) -> int:
    return x + 1


def main() -> int:
    i = 0
    acc = 0
    while i < 500_000:
        acc = inc(acc)
        i += 1
    return acc

