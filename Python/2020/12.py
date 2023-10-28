from lib import *

input = read_input(2020, 12)

lines = input.splitlines()


x = y = 0
dx, dy = 1, 0
for line in lines:
    n = int(line[1:])
    cmd = line[0]
    if cmd == "N":
        y -= n
    if cmd == "E":
        x += n
    if cmd == "S":
        y += n
    if cmd == "W":
        x -= n
    if cmd == "F":
        x += dx * n
        y += dy * n
    if cmd == "L":
        for _ in range(n // 90 % 4):
            dx, dy = dy, -dx
    if cmd == "R":
        for _ in range(n // 90 % 4):
            dx, dy = -dy, dx

print(abs(x) + abs(y))


wx, wy = 10, -1
sx = sy = 0
for line in lines:
    n = int(line[1:])
    cmd = line[0]
    if cmd == "N":
        wy -= n
    if cmd == "E":
        wx += n
    if cmd == "S":
        wy += n
    if cmd == "W":
        wx -= n
    if cmd == "F":
        sx += wx * n
        sy += wy * n
    if cmd == "L":
        for _ in range(n // 90 % 4):
            wx, wy = wy, -wx
    if cmd == "R":
        for _ in range(n // 90 % 4):
            wx, wy = -wy, wx
print(abs(sx) + abs(sy))
