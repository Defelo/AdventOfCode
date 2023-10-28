from lib import *

input = read_input(2021, 13)


dots = {tuple(map(int, line.split(","))) for line in input.split("\n\n")[0].splitlines()}


line = input.split("\n\n")[1].splitlines()[0]

c, n = re.match(r"^fold along (.)=(\d+)$", line).groups()

n = int(n)

dots = {(min(x, 2 * n - x) if c == "x" else x, min(y, 2 * n - y) if c == "y" else y) for x, y in dots}


print(len(dots))


dots = {tuple(map(int, line.split(","))) for line in input.split("\n\n")[0].splitlines()}

for line in input.split("\n\n")[1].splitlines():
    c, n = re.match(r"^fold along (.)=(\d+)$", line).groups()

    n = int(n)

    dots = {(min(x, 2 * n - x) if c == "x" else x, min(y, 2 * n - y) if c == "y" else y) for x, y in dots}

out = ""

n = 0

while True:
    k = 0

    for i in range(5 * n, 5 * n + 4):
        for j in range(6):
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
        0b111111001000010110100001: "K",
        0b111111000001000001000001: "L",
        0b111111100100100100011000: "P",
        0b111111100100100110011001: "R",
        0b111110000001000001111110: "U",
        0b100011100101101001110001: "Z",
    }.get(k, "?")

    n += 1

print(out)
