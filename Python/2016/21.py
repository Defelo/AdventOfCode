from lib import *

input = read_input(2016, 21)

import re


def scramble(inp):
    (*out,) = inp

    for line in input.splitlines():
        if match := re.match(r"^swap position (\d+) with position (\d+)$", line):
            x, y = map(int, match.groups())

            out[x], out[y] = out[y], out[x]

        if match := re.match(r"^swap letter ([a-zA-Z\d]+) with letter ([a-zA-Z\d]+)$", line):
            x, y = match.groups()

            out = [[[c, x][c == y], y][c == x] for c in out]

        if match := re.match(r"^rotate (left|right) (\d+) steps?$", line):
            d, n = match.groups()

            n = int(n)

            if d == "right":
                n *= -1

            n %= len(out)

            out = out[n:] + out[:n]

        if match := re.match(r"^rotate based on position of letter ([a-zA-Z\d]+)$", line):
            (x,) = match.groups()

            idx = out.index(x)

            n = -(1 + idx + (idx >= 4)) % len(out)

            out = out[n:] + out[:n]

        if match := re.match(r"^reverse positions (\d+) through (\d+)$", line):
            x, y = sorted(map(int, match.groups()))

            out = out[:x] + out[x : y + 1][::-1] + out[y + 1 :]

        if match := re.match(r"^move position (\d+) to position (\d+)$", line):
            x, y = map(int, match.groups())

            out.insert(y, out.pop(x))

    return "".join(out)


print(scramble("abcdefgh"))

for it in itertools.permutations("abcdefgh"):
    if scramble(inp := "".join(it)) == "fbgdceah":
        print(inp)
        break
