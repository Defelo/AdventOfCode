from lib import *

input = read_input(2019, 6)

graph = {}
for line in input.splitlines():
    a, b = line.split(")")
    graph.setdefault(a, set()).add(b)

out = 0
Q = [("COM", 0)]
while Q:
    node, count = Q.pop(0)
    out += count
    for child in graph.get(node, set()):
        Q.append((child, count + 1))
print(out)


graph = {}
for line in input.splitlines():
    a, b = line.split(")")
    graph.setdefault(a, set()).add(b)
    graph.setdefault(b, set()).add(a)

Q = [("YOU", 0)]
visited = set()
while Q:
    node, dist = Q.pop(0)

    if node in visited:
        continue
    visited.add(node)

    if node == "SAN":
        print(dist - 2)

    for child in graph.get(node, set()):
        Q.append((child, dist + 1))
