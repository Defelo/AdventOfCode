from lib import *

input = read_input(2017, 12)

lines = input.splitlines()


edges = {}
for line in lines:
    p, Q = line.split(" <-> ")
    p = int(p)
    for q in map(int, Q.split(", ")):
        edges.setdefault(p, []).append(q)

queue = [0]
visited = set()
while queue:
    p = queue.pop(0)

    if p in visited:
        continue

    visited.add(p)

    for q in edges.get(p, []):
        queue.append(q)

print(len(visited))


uf = UnionFind(len(lines))
for line in lines:
    p, Q = line.split(" <-> ")
    p = int(p)
    for q in map(int, Q.split(", ")):
        uf.merge(p, q)

groups = set()
for i in range(len(lines)):
    groups.add(uf.find(i))

print(len(groups))
