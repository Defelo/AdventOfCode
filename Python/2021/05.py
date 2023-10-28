from lib import *

input = read_input(2021, 5)


seen = set()
dup = set()
for line in input.splitlines():
    a, b = line.split(" -> ")
    x1, y1 = map(int, a.split(","))
    x2, y2 = map(int, b.split(","))
    if x1 != x2 and y1 != y2:
        continue

    for x, y in iter_line(x1, y1, x2, y2):
        if (x, y) in seen:
            dup.add((x, y))

        seen.add((x, y))

print(len(dup))


seen = set()
dup = set()
for line in input.splitlines():
    a, b = line.split(" -> ")
    x1, y1 = map(int, a.split(","))
    x2, y2 = map(int, b.split(","))
    for x, y in iter_line(x1, y1, x2, y2):
        if (x, y) in seen:
            dup.add((x, y))

        seen.add((x, y))

print(len(dup))
