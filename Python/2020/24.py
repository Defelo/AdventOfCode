from lib import *

input = read_input(2020, 24)

lines = input.splitlines()

adj = {"e": (1, 0), "w": (-1, 0), "se": (1, 1), "sw": (0, 1), "ne": (0, -1), "nw": (-1, -1)}


def get_tile(line):
    x = y = 0
    i = 0
    while i < len(line):
        if line[i] in "ew":
            dx, dy = adj[line[i]]
            i += 1
        else:
            dx, dy = adj[line[i : i + 2]]
            i += 2
        x += dx
        y += dy
    return x, y


def get_black():
    black = set()
    for line in lines:
        x, y = get_tile(line)
        if (x, y) in black:
            black.remove((x, y))
        else:
            black.add((x, y))
    return black


print(len(get_black()))


def neighbours(x, y):
    return {(x + dx, y + dy) for dx, dy in adj.values()}


black = get_black()
for _ in range(100):
    to_update = [(p, q) for x, y in black for p, q in neighbours(x, y) | {(x, y)}]
    flip_white = set()
    flip_black = set()
    for x, y in to_update:
        cnt = len(black & neighbours(x, y))
        alive = (x, y) in black
        if alive and cnt not in (1, 2):
            flip_white.add((x, y))
        elif not alive and cnt == 2:
            flip_black.add((x, y))
    black -= flip_white
    black |= flip_black

print(len(black))
