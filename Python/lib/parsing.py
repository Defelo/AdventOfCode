import re


def ints(inp):
    return [int(x[0]) for x in re.finditer(r"[+-]?\d+", inp)]


def pints(inp):
    return [int(x[0]) for x in re.finditer(r"\d+", inp)]


def floats(inp):
    return [float(x[0]) for x in re.finditer(r"[+-]?(\d*\.)?\d+", inp)]


def pfloats(inp):
    return [float(x[0]) for x in re.finditer(r"(\d*\.)?\d+", inp)]


def parse_ascii(dots: set[tuple[int, int]]) -> str:
    out = ""
    n = 0
    while True:
        k = 0
        for j in range(5 * n, 5 * n + 4):
            for i in range(6):
                k <<= 1
                k |= (i, j) in dots
        if not k:
            break
        out += {
            0b011111100100100100011111: "A",
            0b111111101001101001010110: "B",
            0b011110100001100001010010: "C",
            0b111111101001101001100001: "E",
            0b111111101000101000100000: "F",
            0b011110100001100101010111: "G",
            0b111111001000001000111111: "H",
            0b000010000001100001111111: "J",
            0b000010000001100001111110: "J",
            0b111111001000010110100001: "K",
            0b111111000001000001000001: "L",
            0b111111100100100100011000: "P",
            0b111111100100100110011001: "R",
            0b111110000001000001111110: "U",
            0b100011100101101001110001: "Z",
        }.get(k, "?")
        n += 1

    return out
