import re

points = []
for line in open("input.txt").read().splitlines():
    x, y, vx, vy = map(int, re.match(r"^position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>$", line).groups())
    points.append((x, y, vx, vy))

i = 0
best = 1e1337
prev = None
while True:
    p = {}
    minx = miny = 1e1337
    maxx = maxy = -1e1337
    for x, y, vx, vy in points:
        x += vx * i
        y += vy * i
        minx = min(x, minx)
        maxx = max(x, maxx)
        miny = min(y, miny)
        maxy = max(y, maxy)
        p[(x, y)] = True
    dist = maxx - minx + maxy - miny

    if dist < best:
        best = dist
        i += 1
        prev = p
    else:
        print(i - 1)
        break
