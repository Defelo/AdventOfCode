from lib import *

input = read_input(2019, 10)


grid = input.splitlines()
height = len(grid)
width = len(grid[0])
assert all(width == len(l) for l in grid)


def check(x, y, x2, y2):
    steps = math.gcd(abs(x2 - x), abs(y2 - y))
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

(*grid,) = map(list, input.splitlines())
height = len(grid)
width = len(grid[0])


def calc_angle(x, y):
    if not x:
        return 0
    return math.atan(y / x) * math.pi / 180 + 90


def check(x, y, x2, y2):
    steps = math.gcd(abs(x2 - x), abs(y2 - y))
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


asteroids = [(x, y) for y in range(height) for x in range(width) if grid[y][x] == "#"]
lx, ly = max(asteroids, key=lambda a: count(*a))
asteroids.remove((lx, ly))
asteroid_vaporization_queue = []
while asteroids:
    visible = [asteroid for asteroid in asteroids if check(lx, ly, *asteroid)]
    visible.sort(key=lambda a: calc_angle(a[0] - lx, a[1] - ly))
    for asteroid in visible:
        x, y = asteroid
        asteroid_vaporization_queue.append(asteroid)
        asteroids.remove(asteroid)
        grid[y][x] = "."
x, y = asteroid_vaporization_queue[199]
print(x * 100 + y)
