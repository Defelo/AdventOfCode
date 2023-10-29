from lib import *

input = read_input(2017, 11).strip()


def step(x, y, d):
    if d == "n":
        return x, y - 1  # (0, -1)
    if d == "ne":
        return x + 1, y  # (1, 0)
    if d == "se":
        return x + 1, y + 1  # (1, 1)
    if d == "s":
        return x, y + 1  # (0, 1)
    if d == "sw":
        return x - 1, y  # (-1, 0)
    if d == "nw":
        return x - 1, y - 1  # (-1, -1)


x, y = 0, 0
for d in input.split(","):
    x, y = step(x, y, d)
k = 0
if x * y > 0:
    k = min(x, y, key=abs)
    x -= k
    y -= k
print(abs(x) + abs(y) + k)


x, y = 0, 0
out = 0
for d in input.split(","):
    x, y = step(x, y, d)
    k = 0
    if x * y > 0:
        k = min(x, y, key=abs)
        x -= k
        y -= k
    out = max(out, abs(x) + abs(y) + k)
print(out)
