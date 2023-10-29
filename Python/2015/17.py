from lib import *

input = read_input(2015, 17)

containers = list(map(int, input.splitlines()))

dp = {}


def solve1(i, x):
    if i == len(containers):
        return int(x == 0)

    if (i, x) not in dp:
        out = solve1(i + 1, x)

        if containers[i] <= x:
            out += solve1(i + 1, x - containers[i])

        dp[(i, x)] = out

    return dp[(i, x)]


print(solve1(0, 150))


containers = list(map(int, input.splitlines()))
dp = {}


def solve2(i, x):
    if i == len(containers):
        return None if x else (0, 1)

    if (i, x) not in dp:
        out = solve2(i + 1, x)

        if containers[i] <= x:
            y = solve2(i + 1, x - containers[i])

            if y is not None:
                l, c = y

                l += 1

                if out is None or l < out[0]:
                    out = l, c

                elif out is not None and out[0] == l:
                    out = l, out[1] + c

        dp[(i, x)] = out

    return dp[(i, x)]


print(solve2(0, 150)[1])
