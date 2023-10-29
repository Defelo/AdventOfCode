from lib import *

input = read_input(2017, 19)

lines = input.splitlines()


w, h = len(lines[0]), len(lines)
x, y = lines[0].index("|"), 0
dx, dy = 0, 1
out = ""
get = lambda x, y: lines[y][x] if y in range(h) and x in range(w) else " "
while True:
    c = get(x, y)
    f = get(x + dx, y + dy)
    l = get(x + dy, y - dx)
    r = get(x - dy, y + dx)
    if c.isalpha():
        out += c
    if f == " ":
        if l != " ":
            dx, dy = dy, -dx
        elif r != " ":
            dx, dy = -dy, dx
        else:
            break
    x += dx
    y += dy

print(out)


w, h = len(lines[0]), len(lines)
x, y = lines[0].index("|"), 0
dx, dy = 0, 1
out = 0
get = lambda x, y: lines[y][x] if y in range(h) and x in range(w) else " "
while True:
    out += 1
    f = get(x + dx, y + dy)
    l = get(x + dy, y - dx)
    r = get(x - dy, y + dx)
    if f == " ":
        if l != " ":
            dx, dy = dy, -dx
        elif r != " ":
            dx, dy = -dy, dx
        else:
            break
    x += dx
    y += dy

print(out)
