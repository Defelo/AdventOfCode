graph = {}
for line in open("input.txt").read().splitlines():
    a, b = line.split(")")
    graph.setdefault(a, set()).add(b)
    graph.setdefault(b, set()).add(a)

Q = [("YOU", 0)]
visited = set()
while Q:
    node, dist = Q.pop(0)

    if node in visited: continue
    visited.add(node)

    if node == "SAN":
        print(dist - 2)

    for child in graph.get(node, set()):
        Q.append((child, dist + 1))
