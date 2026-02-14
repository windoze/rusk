EXPECTED = 45_001_350_000


class Point:
    __slots__ = ("x", "y")

    def __init__(self, x: int, y: int) -> None:
        self.x = x
        self.y = y


def tuple_loop(n: int) -> int:
    t = [1, 2, 3]
    i = 0
    total = 0
    while i < n:
        t[0] = t[0] + 1
        t[1] = t[1] + 2
        t[2] = t[2] + 3
        total = total + t[0] + t[1] + t[2]
        i += 1
    return total


def struct_loop(n: int) -> int:
    p = Point(1, 2)
    i = 0
    total = 0
    while i < n:
        p.x = p.x + 1
        p.y = p.y + 2
        total = total + p.x + p.y
        i += 1
    return total


def main() -> int:
    return struct_loop(100_000) + tuple_loop(100_000)

