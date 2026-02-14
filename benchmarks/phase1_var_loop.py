EXPECTED = 20_000_300_000


def main() -> int:
    n = 200_000
    i = 0
    x = 0
    y = 0
    while i < n:
        x += 1
        y += x
        i += 1
    return x + y
