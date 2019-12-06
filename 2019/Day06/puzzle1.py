graph = {}
for line in open("input.txt").read().splitlines():
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
