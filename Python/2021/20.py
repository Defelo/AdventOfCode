from lib import *

input = read_input(2021, 20)

lines = input.splitlines()


def solve(n):
    algo = [c == "#" for c in lines[0]]
    grid = {(j, i) for i, line in enumerate(lines[2:]) for j, x in enumerate(line) if x == "#"}
    inf = False
    for _ in range(n):
        new_inf = inf != algo[0]
        candidates = {(p, q) for x, y in grid for p, q in get_neighbors(x, y, diag=True, include_self=True)}
        new_grid = set()
        for x, y in candidates:
            idx = 0

            for p, q in sorted(get_neighbors(x, y, diag=True, include_self=True), key=lambda a: a[::-1]):
                idx <<= 1
                idx |= ((p, q) in grid) != inf

            if algo[idx] != new_inf:
                new_grid.add((x, y))

        grid = new_grid
        inf = new_inf

    return len(grid)


print(solve(2))
print(solve(50))
