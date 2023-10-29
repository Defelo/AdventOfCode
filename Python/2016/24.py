from lib import *

input = read_input(2016, 24)


grid = input.splitlines()

nodes = {}

rnodes = {}

for i, line in enumerate(grid):
    for j, c in enumerate(line):
        if c.isnumeric():
            nodes[int(c)] = j, i

            rnodes[(j, i)] = int(c)


def asp(x, y):
    queue = [(0, x, y)]

    out = {}

    visited = set()

    while queue:
        d, x, y = queue.pop(0)

        if (x, y) in visited:
            continue

        visited.add((x, y))

        if (x, y) in rnodes:
            out[rnodes[(x, y)]] = d

        for p, q in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]:
            if p not in range(len(grid[0])) or q not in range(len(grid)) or grid[q][p] == "#":
                continue

            queue.append((d + 1, p, q))

    return out


sp = {k: asp(*v) for k, v in nodes.items()}

best = 1e1337

for order in itertools.permutations(set(sp) - {0}):
    pos = 0

    cost = 0

    for x in order:
        cost += sp[pos][x]

        pos = x

    best = min(best, cost)

print(best)


best = 1e1337

for order in itertools.permutations(set(sp) - {0}):
    pos = 0

    cost = 0

    for x in order:
        cost += sp[pos][x]

        pos = x

    best = min(best, cost + sp[pos][0])

print(best)
