from lib import *

input = read_input(2016, 22)

size = {}

used = {}


for line in input.splitlines()[2:]:
    node, s, u, *_ = line.split()

    node = tuple(int(x[1:]) for x in node.split("-")[1:])

    s, u = [int(x[:-1]) for x in (s, u)]

    size[node] = s

    used[node] = u


out = 0

for p1 in size:
    if not used[p1]:
        continue

    for p2 in size:
        if p1 == p2:
            continue

        out += used[p1] + used[p2] <= size[p2]

print(out)


width, height = max((x + 1, y + 1) for x, y in size)

target = width - 1


def free(x, y):
    queue = [(x, y, [])]

    visited = set()

    while queue:
        x, y, prev = queue.pop(0)

        if not used[(x, y)]:
            for p, q in prev[::-1]:
                used[(x, y)] = used[(p, q)]

                used[(p, q)] = 0

                x, y = p, q

            return len(prev)

        if (x, y) in visited:
            continue

        visited.add((x, y))

        for p, q in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]:
            if p not in range(width) or q not in range(height):
                continue

            if (p, q) == (target, 0):
                continue

            if (p, q) in visited:
                continue

            if used[(x, y)] > size[(p, q)]:
                continue

            queue.append((p, q, prev + [(x, y)]))


out = 0
while target:
    out += free(target - 1, 0)
    used[(target - 1, 0)] = used[(target, 0)]
    used[(target, 0)] = 0
    target -= 1
    out += 1

print(out)
