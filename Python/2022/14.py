from lib import *

input = read_input(2022, 14)


rock = set()
maxy = 0
for line in input.splitlines():
    for a, b in sliding_window(line.split(" -> ")):
        for x, y in iter_line(*map(int, a.split(",")), *map(int, b.split(","))):
            rock.add((x, y))
            maxy = max(y, maxy)


def simulate(x, y, air, maxy):
    while y <= maxy:
        if air(x, y + 1):
            y += 1
        elif air(x - 1, y + 1):
            x -= 1
            y += 1
        elif air(x + 1, y + 1):
            x += 1
            y += 1
        else:
            return x, y
    return None


sand = set()
while s := simulate(500, 0, lambda x, y: (x, y) not in rock and (x, y) not in sand, maxy):
    sand.add(s)
print(len(sand))


sand = set()
while (500, 0) not in sand:
    sand.add(simulate(500, 0, lambda x, y: y < maxy + 2 and (x, y) not in rock and (x, y) not in sand, maxy + 2))
print(len(sand))
