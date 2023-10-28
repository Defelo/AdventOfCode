from lib import *

input = read_input(2020, 11)

lines = input.splitlines()

graph = {}
active = set()
for y, line in enumerate(lines):
    for x, c in enumerate(line):
        if c == ".":
            continue
        graph[(x, y)] = []
        for dy in range(-1, 2):
            for dx in range(-1, 2):
                if dx == 0 == dy:
                    continue
                if 0 <= (i := y + dy) < len(lines) and 0 <= (j := x + dx) < len(line) and lines[i][j] != ".":
                    graph[(x, y)].append((j, i))

while True:
    new_active = set()
    for p, qs in graph.items():
        cnt = sum(q in active for q in qs)
        if p not in active and not cnt or p in active and cnt < 4:
            new_active.add(p)
    if active == new_active:
        break
    active = new_active

print(len(active))


graph = {}
active = set()
for y, line in enumerate(lines):
    for x, c in enumerate(line):
        if c == ".":
            continue
        graph[(x, y)] = []
        for dy in range(-1, 2):
            for dx in range(-1, 2):
                if dx == 0 == dy:
                    continue
                k = 1
                while 0 <= (i := y + k * dy) < len(lines) and 0 <= (j := x + k * dx) < len(line):
                    if lines[i][j] != ".":
                        graph[(x, y)].append((j, i))
                        break
                    k += 1

while True:
    new_active = set()
    for p, qs in graph.items():
        cnt = sum(q in active for q in qs)
        if p not in active and not cnt or p in active and cnt < 5:
            new_active.add(p)
    if active == new_active:
        break
    active = new_active

print(len(active))
