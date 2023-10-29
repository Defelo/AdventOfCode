from lib import *

input = read_input(2018, 18)


def cntAdj(grid, y, x, t):
    return sum(
        grid[i][j] == t
        for i in range(y - 1, y + 2)
        for j in range(x - 1, x + 2)
        if (i, j) != (y, x) and i in range(len(grid)) and j in range(len(grid[i]))
    )


grid = input.splitlines()
for _ in range(10):
    new_grid = []
    for i, line in enumerate(grid):
        new_line = ""
        for j, c in enumerate(line):
            if c == "." and cntAdj(grid, i, j, "|") >= 3:
                new_line += "|"
            elif c == "|" and cntAdj(grid, i, j, "#") >= 3:
                new_line += "#"
            elif c == "#" and not (cntAdj(grid, i, j, "#") >= 1 and cntAdj(grid, i, j, "|") >= 1):
                new_line += "."
            else:
                new_line += c
        new_grid.append(new_line)
    grid = new_grid

a = sum(line.count("|") for line in grid)
b = sum(line.count("#") for line in grid)
print(a * b)


def next_iteration(grid):
    new_grid = []
    for i, line in enumerate(grid):
        new_line = ""
        for j, c in enumerate(line):
            if c == "." and cntAdj(grid, i, j, "|") >= 3:
                new_line += "|"
            elif c == "|" and cntAdj(grid, i, j, "#") >= 3:
                new_line += "#"
            elif c == "#" and not (cntAdj(grid, i, j, "#") >= 1 and cntAdj(grid, i, j, "|") >= 1):
                new_line += "."
            else:
                new_line += c
        new_grid.append(new_line)
    return new_grid


grid = input.splitlines()
seen = {}
i = 0
while tuple(grid) not in seen:
    seen[tuple(grid)] = i
    grid = next_iteration(grid)
    i += 1

cycle = i - seen[tuple(grid)]
for _ in range((1000000000 - i) % cycle):
    grid = next_iteration(grid)

a = sum(line.count("|") for line in grid)
b = sum(line.count("#") for line in grid)
print(a * b)
