from lib import *

input = read_input(2018, 15)

lines = input.splitlines()


def adj(x, y):
    return [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]


entities = {}
walls = set()
idx = 0
for i, line in enumerate(lines):
    for j, c in enumerate(line):
        if c == "#":
            walls.add((j, i))
        elif c != ".":
            entities[(j, i)] = (idx, c == "E", 200)
            idx += 1

rnd = 0
while True:
    done = False
    orig = entities.copy()
    for x, y in sorted(entities, key=lambda a: a[::-1]):
        if (x, y) not in entities:
            continue
        idx, elf, hp = entities[(x, y)]
        if idx != orig[(x, y)][0]:
            continue
        if not any(q[1] != elf for q in entities.values()):
            done = True
            break

        in_range = [(p, q) for p in adj(x, y) if (q := entities.get(p)) and q[1] != elf]
        if not in_range:
            queue = [(0, x, y)]
            visited = set()
            dist = None
            nearest = []
            while queue:
                d, p, q = queue.pop(0)
                if (p, q) in visited:
                    continue

                visited.add((p, q))
                if any(e[1] != elf for r in adj(p, q) if (e := entities.get(r))):
                    if dist is None:
                        dist = d
                    elif d > dist:
                        break
                    nearest.append((p, q))

                for r in adj(p, q):
                    if r in walls:
                        continue
                    if (e := entities.get(r)) and e[1] == elf:
                        continue
                    queue.append((d + 1, *r))

            if not nearest:
                continue

            target = min(nearest, key=lambda a: a[::-1])
            queue = [(0, *target)]
            visited = set()
            dist = None
            nearest = []
            while queue:
                d, p, q = queue.pop(0)
                if (p, q) in visited:
                    continue

                visited.add((p, q))
                if (x, y) in adj(p, q):
                    if dist is None:
                        dist = d
                    elif d > dist:
                        break

                    nearest.append((p, q))

                for r in adj(p, q):
                    if r in walls:
                        continue
                    if (e := entities.get(r)) and e[1] == elf:
                        continue

                    queue.append((d + 1, *r))

            if not nearest:
                continue

            g = min(nearest, key=lambda a: a[::-1])
            entities[g] = entities.pop((x, y))
            x, y = g

        in_range = [(p, q) for p in adj(x, y) if (q := entities.get(p)) and q[1] != elf]

        if in_range:
            in_range.sort(key=lambda a: (a[1][2], a[0][::-1]))
            p, (idx2, elf2, hp2) = in_range[0]
            hp2 -= 3
            if hp2 <= 0:
                entities.pop(p)
            else:
                entities[p] = idx2, elf2, hp2

    if done:
        break

    rnd += 1

print(rnd * sum(e[2] for e in entities.values()))


def simulate(ap):
    entities = {}
    walls = set()
    idx = 0
    for i, line in enumerate(lines):
        for j, c in enumerate(line):
            if c == "#":
                walls.add((j, i))
            elif c != ".":
                entities[(j, i)] = (idx, c == "E", 200)
                idx += 1

    rnd = 0
    while True:
        done = False

        orig = entities.copy()
        for x, y in sorted(entities, key=lambda a: a[::-1]):
            if (x, y) not in entities:
                continue

            idx, elf, hp = entities[(x, y)]
            if idx != orig[(x, y)][0]:
                continue

            if not any(q[1] != elf for q in entities.values()):
                done = True
                break

            in_range = [(p, q) for p in adj(x, y) if (q := entities.get(p)) and q[1] != elf]
            if not in_range:
                queue = [(0, x, y)]
                visited = set()
                dist = None
                nearest = []
                while queue:
                    d, p, q = queue.pop(0)
                    if (p, q) in visited:
                        continue

                    visited.add((p, q))
                    if any(e[1] != elf for r in adj(p, q) if (e := entities.get(r))):
                        if dist is None:
                            dist = d
                        elif d > dist:
                            break
                        nearest.append((p, q))
                    for r in adj(p, q):
                        if r in walls:
                            continue
                        if (e := entities.get(r)) and e[1] == elf:
                            continue

                        queue.append((d + 1, *r))

                if not nearest:
                    continue

                target = min(nearest, key=lambda a: a[::-1])
                queue = [(0, *target)]
                visited = set()
                dist = None
                nearest = []
                while queue:
                    d, p, q = queue.pop(0)
                    if (p, q) in visited:
                        continue

                    visited.add((p, q))
                    if (x, y) in adj(p, q):
                        if dist is None:
                            dist = d
                        elif d > dist:
                            break
                        nearest.append((p, q))

                    for r in adj(p, q):
                        if r in walls:
                            continue
                        if (e := entities.get(r)) and e[1] == elf:
                            continue
                        queue.append((d + 1, *r))

                if not nearest:
                    continue

                g = min(nearest, key=lambda a: a[::-1])
                entities[g] = entities.pop((x, y))
                x, y = g

            in_range = [(p, q) for p in adj(x, y) if (q := entities.get(p)) and q[1] != elf]
            if in_range:
                in_range.sort(key=lambda a: (a[1][2], a[0][::-1]))
                p, (idx2, elf2, hp2) = in_range[0]
                hp2 -= ap if elf else 3
                if hp2 <= 0:
                    if elf2:
                        return -1

                    entities.pop(p)
                else:
                    entities[p] = idx2, elf2, hp2

        if done:
            break

        rnd += 1

    return rnd * sum(e[2] for e in entities.values())


ap = 4
while (res := simulate(ap)) < 0:
    ap += 1
print(res)
