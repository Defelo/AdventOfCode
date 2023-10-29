from lib import *

input = read_input(2016, 1)

x = y = 0
dx, dy = 1, 0
for e in input.split(", "):
    l = int(e[1:])
    if e[0] == "L":
        dx, dy = dy, -dx
    else:
        dx, dy = -dy, dx

    x += l * dx
    y += l * dy

print(abs(x) + abs(y))


x = y = 0
dx, dy = 1, 0
seen = {(0, 0)}
for e in input.split(", "):
    l = int(e[1:])
    if e[0] == "L":
        dx, dy = dy, -dx
    else:
        dx, dy = -dy, dx

    for i in range(l):
        x += dx
        y += dy
        if (x, y) in seen:
            print(abs(x) + abs(y))
            break
        seen.add((x, y))
    else:
        continue
    break
