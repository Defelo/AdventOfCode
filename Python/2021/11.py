from lib import *

input = read_input(2021, 11)


lines = input.splitlines()
grid = [[*map(int, line)] for line in lines]
out = 0
for _ in range(100):
    flashes = []
    visited = set()
    for i, row in enumerate(grid):
        for j, x in enumerate(row):
            if x >= 9:
                flashes.append((i, j))
            else:
                row[j] += 1

    while flashes:
        i, j = flashes.pop(0)
        if (i, j) in visited:
            continue

        visited.add((i, j))
        grid[i][j] = 0
        out += 1
        for q, p in get_neighbors(j, i, len(lines[0]), len(lines), diag=True):
            x = grid[p][q]
            if x >= 9:
                flashes.append((p, q))
            elif (p, q) not in visited:
                grid[p][q] += 1

print(out)


grid = [[*map(int, line)] for line in lines]

for step in irange():
    flashes = []

    visited = set()

    for i, row in enumerate(grid):
        for j, x in enumerate(row):
            if x >= 9:
                flashes.append((i, j))

            else:
                row[j] += 1

    while flashes:
        i, j = flashes.pop(0)

        if (i, j) in visited:
            continue

        visited.add((i, j))

        grid[i][j] = 0

        for q, p in get_neighbors(j, i, len(lines[0]), len(lines), diag=True):
            x = grid[p][q]

            if x >= 9:
                flashes.append((p, q))

            elif (p, q) not in visited:
                grid[p][q] += 1

    if len(visited) == len(lines) * len(lines[0]):
        print(step + 1)
        break
