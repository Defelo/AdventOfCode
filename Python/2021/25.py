from lib import *

input = read_input(2021, 25)

lines = input.splitlines()


right = set()
down = set()
width = len(lines[0])
height = len(lines)
for i, line in enumerate(lines):
    for j, c in enumerate(line):
        match c:
            case "v":
                down.add((j, i))
            case ">":
                right.add((j, i))

i = 0
while True:
    old = {*right}, {*down}
    right = {
        new_pos if (new_pos := ((x + 1) % width, y)) not in right and new_pos not in down else (x, y) for x, y in right
    }
    down = {
        new_pos if (new_pos := (x, (y + 1) % height)) not in right and new_pos not in down else (x, y) for x, y in down
    }
    i += 1
    if (right, down) == old:
        break

print(i)
