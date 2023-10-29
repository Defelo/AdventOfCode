from lib import *

input = read_input(2016, 2)

out = []
x = y = 1
for line in input.splitlines():
    for c in line:
        if c == "L" and x > 0:
            x -= 1

        elif c == "R" and x < 2:
            x += 1

        elif c == "U" and y > 0:
            y -= 1

        elif c == "D" and y < 2:
            y += 1

    out.append(y * 3 + x + 1)

print(*out, sep="")


out = ""
x, y = 0, 2
keypad = ["  1  ", " 234 ", "56789", " ABC ", "  D  "]
for line in input.splitlines():
    for c in line:
        nx, ny = x, y

        if c == "L":
            nx -= 1

        elif c == "R":
            nx += 1

        elif c == "U":
            ny -= 1

        elif c == "D":
            ny += 1

        if 0 <= nx < 5 and 0 <= ny < 5 and keypad[ny][nx] != " ":
            x, y = nx, ny

    out += keypad[y][x]

print(out)
