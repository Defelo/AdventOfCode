from lib import *

input = read_input(2018, 13)


carts = []
tracks = input.splitlines()
positions = set()
for y, line in enumerate(tracks):
    for x in range(len(line)):
        c = line[x]
        if c == "^":
            carts.append([x, y, 0, -1, 0])
            positions.add((x, y))
            tracks[y] = line = line[:x] + "|" + line[x + 1 :]
        elif c == "v":
            carts.append([x, y, 0, 1, 0])
            positions.add((x, y))
            tracks[y] = line = line[:x] + "|" + line[x + 1 :]
        elif c == "<":
            carts.append([x, y, -1, 0, 0])
            positions.add((x, y))
            tracks[y] = line = line[:x] + "-" + line[x + 1 :]
        elif c == ">":
            carts.append([x, y, 1, 0, 0])
            positions.add((x, y))
            tracks[y] = line = line[:x] + "-" + line[x + 1 :]

while True:
    for cart in sorted(carts, key=lambda c: (c[1], c[0])):
        x, y, dx, dy, k = cart
        positions.remove((x, y))
        x += dx
        y += dy
        if (x, y) in positions:
            print(f"{x},{y}")
            break
        positions.add((x, y))
        if tracks[y][x] == "/":
            dx, dy = -dy, -dx
        elif tracks[y][x] == "\\":
            dx, dy = dy, dx
        elif tracks[y][x] == "+":
            if k == 0:
                dx, dy = dy, -dx
            elif k == 2:
                dx, dy = -dy, dx
            k = (k + 1) % 3
        cart[:] = x, y, dx, dy, k
    else:
        continue
    break


carts = []
tracks = input.splitlines()
positions = {}
crashed = set()
for y, line in enumerate(tracks):
    for x in range(len(line)):
        c = line[x]
        if c == "^":
            carts.append([x * len(tracks) + y, x, y, 0, -1, 0])
            positions[(x, y)] = x * len(tracks) + y
            tracks[y] = line = line[:x] + "|" + line[x + 1 :]
        elif c == "v":
            carts.append([x * len(tracks) + y, x, y, 0, 1, 0])
            positions[(x, y)] = x * len(tracks) + y
            tracks[y] = line = line[:x] + "|" + line[x + 1 :]
        elif c == "<":
            carts.append([x * len(tracks) + y, x, y, -1, 0, 0])
            positions[(x, y)] = x * len(tracks) + y
            tracks[y] = line = line[:x] + "-" + line[x + 1 :]
        elif c == ">":
            carts.append([x * len(tracks) + y, x, y, 1, 0, 0])
            positions[(x, y)] = x * len(tracks) + y
            tracks[y] = line = line[:x] + "-" + line[x + 1 :]

while True:
    for cart in sorted(carts, key=lambda c: (c[2], c[1])):
        i, x, y, dx, dy, k = cart
        if i in crashed:
            continue
        positions.pop((x, y))
        x += dx
        y += dy
        if (x, y) in positions:
            crashed.add(i)
            crashed.add(positions.pop((x, y)))
            continue
        positions[(x, y)] = i
        if tracks[y][x] == "/":
            dx, dy = -dy, -dx
        elif tracks[y][x] == "\\":
            dx, dy = dy, dx
        elif tracks[y][x] == "+":
            if k == 0:
                dx, dy = dy, -dx
            elif k == 2:
                dx, dy = -dy, dx
            k = (k + 1) % 3
        cart[:] = i, x, y, dx, dy, k
    if len(carts) - len(crashed) == 1:
        for i, x, y, *_ in carts:
            if i not in crashed:
                print(f"{x},{y}")
                break
        else:
            continue
        break
