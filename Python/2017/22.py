from lib import *

input = read_input(2017, 22)

lines = input.splitlines()


infected = set()
x, y = len(lines[0]) // 2, len(lines) // 2
dx, dy = 0, -1
out = 0
for i, line in enumerate(lines):
    for j, c in enumerate(line):
        if c == "#":
            infected.add((j, i))

for _ in range(10000):
    inf = (x, y) in infected
    if inf:
        dx, dy = -dy, dx
        infected.remove((x, y))
    else:
        dx, dy = dy, -dx
        infected.add((x, y))
        out += 1
    x += dx
    y += dy

print(out)


state = {}
x, y = len(lines[0]) // 2, len(lines) // 2
dx, dy = 0, -1
out = 0
for i, line in enumerate(lines):
    for j, c in enumerate(line):
        if c == "#":
            state[(j, i)] = 2

for _ in range(10000000):
    s = state.get((x, y), 0)
    if s == 0:
        dx, dy = dy, -dx
    elif s == 2:
        dx, dy = -dy, dx
    elif s == 3:
        dx, dy = -dx, -dy
    if s == 1:
        out += 1
    if s == 3:
        state.pop((x, y))
    else:
        state[(x, y)] = s + 1
    x += dx
    y += dy

print(out)
