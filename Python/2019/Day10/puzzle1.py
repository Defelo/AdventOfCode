from math import gcd

grid = open("input.txt").read().splitlines()
height = len(grid)
width = len(grid[0])
assert all(width == len(l) for l in grid)

def check(x, y, x2, y2):
    steps = gcd(abs(x2 - x), abs(y2 - y))
    xstep = (x2 - x) // steps
    ystep = (y2 - y) // steps
    for _ in range(steps - 1):
        x += xstep
        y += ystep
        if grid[y][x] == "#":
            return False
    return True

def count(x, y):
    out = 0
    for y2 in range(height):
        for x2 in range(width):
            if (x != x2 or y != y2) and grid[y2][x2] == "#" and check(x, y, x2, y2):
                out += 1
    return out

print(max(count(x, y) for y in range(height) for x in range(width) if grid[y][x] == "#"))
