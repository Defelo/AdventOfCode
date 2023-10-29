from lib import *

input = read_input(2015, 15)

TOTAL = 100


ingredients = [[int(x.strip(",")) for x in i[2:-1:2]] for i in map(str.split, input.splitlines())]


def solve1(i, x, prev):
    if i == len(ingredients) - 1:
        out = 1

        for p, e in zip(prev, ingredients[-1]):
            out *= max(0, p + x * e)

        return out

    return max(solve1(i + 1, x - j, [a + j * b for a, b in zip(prev, ingredients[i])]) for j in range(x + 1))


print(solve1(0, TOTAL, [0] * 4))


ingredients = [[int(x.strip(",")) for x in i[2::2]] for i in map(str.split, input.splitlines())]


def solve2(i, x, prev):
    if i == len(ingredients) - 1:
        if prev[-1] + x * ingredients[-1][-1] != 500:
            return 0

        out = 1

        for p, e in zip(prev[:-1], ingredients[-1][:-1]):
            out *= max(0, p + x * e)

        return out

    return max(solve2(i + 1, x - j, [a + j * b for a, b in zip(prev, ingredients[i])]) for j in range(x + 1))


print(solve2(0, TOTAL, [0] * 5))
