from lib import *

input = read_input(2022, 8)

grid = [[*map(int, line)] for line in input.splitlines()]


w, h = len(grid[0]), len(grid)
visible = set()
for dx, dy in NEIGH_DIRECT:
    for y in range(h):
        for x in range(w):
            i = y + dy
            j = x + dx
            while i in range(h) and j in range(w):
                if grid[i][j] >= grid[y][x]:
                    break
                i += dy
                j += dx
            else:
                visible.add((x, y))
print(len(visible))

out = 0
for y in range(len(grid)):
    for x in range(len(grid[y])):
        d = 1
        for dx, dy in NEIGH_DIRECT:
            i = y + dy
            j = x + dx
            s = 0
            while i in range(len(grid)) and j in range(len(grid[0])):
                s += 1
                if grid[i][j] >= grid[y][x]:
                    break
                i += dy
                j += dx
            d *= s
        out = max(out, d)
print(out)
