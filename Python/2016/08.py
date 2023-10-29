from lib import *

input = read_input(2016, 8)


T = lambda g: [*map(list, zip(*g))]


def rotate(g, a, b):
    g[a] = [g[a][(i - b) % len(g[a])] for i in range(len(g[a]))]
    return g


grid = [[0 for _ in range(50)] for _ in range(6)]

for line in input.splitlines():
    if match := re.match(r"^rect (\d+)x(\d+)$", line):
        w, h = map(int, match.groups())

        for i in range(h):
            for j in range(w):
                grid[i][j] = 1

    elif match := re.match(r"^rotate row y=(\d+) by (\d+)$", line):
        a, b = map(int, match.groups())

        rotate(grid, a, b)

    elif match := re.match(r"^rotate column x=(\d+) by (\d+)$", line):
        a, b = map(int, match.groups())

        grid = T(rotate(T(grid), a, b))

print(sum(map(sum, grid)))
for line in grid:
    print("".join(" #"[c] * 2 for c in line))
