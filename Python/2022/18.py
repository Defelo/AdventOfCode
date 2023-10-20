from lib import *

input = read_input(2022, 18)


cubes = [(x, y, z) for x, y, z in map(ints, input.splitlines())]


out = 0
for i, x in enumerate(cubes):
    out += 6
    for y in cubes[:i]:
        if abs(x[0] - y[0]) + abs(x[1] - y[1]) + abs(x[2] - y[2]) == 1:
            out -= 2
print(out)


out = 0
(minx, miny, minz) = (maxx, maxy, maxz) = cubes[0]
for i, x in enumerate(cubes):
    out += 6
    for y in cubes[:i]:
        if abs(x[0] - y[0]) + abs(x[1] - y[1]) + abs(x[2] - y[2]) == 1:
            out -= 2
    minx = min(x[0], minx)
    miny = min(x[1], miny)
    minz = min(x[2], minz)
    maxx = max(x[0], maxx)
    maxy = max(x[1], maxy)
    maxz = max(x[2], maxz)

checked = set()
candidates = [
    (i, j, k)
    for x, y, z in cubes
    for i, j, k in [(x, y, z - 1), (x, y, z + 1), (x, y - 1, z), (x, y + 1, z), (x - 1, y, z), (x + 1, y, z)]
    if (i, j, k) not in cubes
]
while candidates:
    queue = [candidates.pop()]
    if queue[0] in checked:
        continue
    visited = set()
    while queue:
        x, y, z = queue.pop(0)
        if x not in range(minx, maxx + 1) or y not in range(miny, maxy + 1) or z not in range(minz, maxz + 1):
            break
        if (x, y, z) in visited:
            continue
        visited.add((x, y, z))
        for q in [(x, y, z - 1), (x, y, z + 1), (x, y - 1, z), (x, y + 1, z), (x - 1, y, z), (x + 1, y, z)]:
            if q not in cubes and q not in visited:
                queue.append(q)
    else:
        for x, y, z in visited:
            for q in [(x, y, z - 1), (x, y, z + 1), (x, y - 1, z), (x, y + 1, z), (x - 1, y, z), (x + 1, y, z)]:
                if q in cubes:
                    out -= 1

    checked.update(visited)

print(out)
