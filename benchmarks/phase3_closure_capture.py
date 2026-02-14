EXPECTED = 4_999_950_063


def main() -> int:
    # Visible bindings that the lambda does not use.
    a00 = 0
    a01 = 1
    a02 = 2
    a03 = 3
    a04 = 4
    a05 = 5
    a06 = 6
    a07 = 7
    a08 = 8
    a09 = 9
    a10 = 10
    a11 = 11
    a12 = 12
    a13 = 13
    a14 = 14
    a15 = 15
    a16 = 16
    a17 = 17
    a18 = 18
    a19 = 19
    a20 = 20
    a21 = 21
    a22 = 22
    a23 = 23
    a24 = 24
    a25 = 25
    a26 = 26
    a27 = 27
    a28 = 28
    a29 = 29
    a30 = 30
    a31 = 31
    a32 = 32
    a33 = 33
    a34 = 34
    a35 = 35
    a36 = 36
    a37 = 37
    a38 = 38
    a39 = 39
    a40 = 40
    a41 = 41
    a42 = 42
    a43 = 43
    a44 = 44
    a45 = 45
    a46 = 46
    a47 = 47
    a48 = 48
    a49 = 49
    a50 = 50
    a51 = 51
    a52 = 52
    a53 = 53
    a54 = 54
    a55 = 55
    a56 = 56
    a57 = 57
    a58 = 58
    a59 = 59
    a60 = 60
    a61 = 61
    a62 = 62
    a63 = 63

    f = lambda x: x
    i = 0
    total = 0
    while i < 100_000:
        total += f(i)
        i += 1

    return total + a00 + a63

