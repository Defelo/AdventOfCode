from lib import *

input = read_input(2021, 9)


out = 0
lines = input.splitlines()
for i, line in enumerate(lines):
    for j, c in enumerate(lines[i]):
        if all(c < lines[q][p] for p, q in get_neighbors(j, i, len(line), len(lines))):
            out += int(c) + 1

print(out)


sizes = []
for i, line in enumerate(lines):
    for j, c in enumerate(lines[i]):
        if all(c < lines[q][p] for p, q in get_neighbors(j, i, len(line), len(lines))):
            queue = [(i, j)]

            visited = set()

            while queue:
                y, x = queue.pop(0)

                if int(lines[y][x]) == 9:
                    continue

                if (y, x) in visited:
                    continue

                visited.add((y, x))

                for p, q in get_neighbors(x, y, len(line), len(lines)):
                    queue.append((q, p))

            sizes.append(len(visited))

sizes.sort(reverse=True)
print(product(sizes[:3]))
