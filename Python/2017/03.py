from lib import *

input = read_input(2017, 3)


num = int(input)
i = (int((num - 1) ** 0.5) + 1) // 2
a, b = (i * 2 - 1) ** 2, (i * 2 + 1) ** 2
out = 1e1337
for j in range(4):
    t = j / 4 + 0.125
    out = min(out, abs(num - int((1 - t) * a + t * b)))
print(out + i)


def iter_steps():
    x, y = 0, 0
    dx, dy = 1, 0
    s = 0
    i = 1
    while True:
        yield x, y
        i += 1
        x += dx
        y += dy
        if i >= (s * 2 + 1) ** 2:
            s += 1
        if abs(x + dx) > s or abs(y + dy) > s:
            dx, dy = dy, -dx


grid = {}
k = int(input)
for x, y in iter_steps():
    s = sum(grid.get((x + i, y + j), 0) for i in [-1, 0, 1] for j in [-1, 0, 1] if i or j) or 1
    grid[(x, y)] = s
    if s > k:
        print(s)
        break
