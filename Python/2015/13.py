from lib import *

input = read_input(2015, 13)

hp = {}


for a, _, m, c, *_, b in map(str.split, input.splitlines()):
    b = b[:-1]

    c = int(c)

    if m == "lose":
        c *= -1

    hp.setdefault(b, {})[a] = hp[a][b] = hp.setdefault(a, {}).get(b, 0) + c


def calc(table):
    return sum(hp.get(table[i], {}).get(table[(i + 1) % len(table)], 0) for i in range(len(table)))


print(max(map(calc, itertools.permutations(hp))))
print(max(map(calc, itertools.permutations([*hp, "_"]))))
