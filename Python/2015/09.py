from lib import *

input = read_input(2015, 9)


graph = {}
for src, _, dst, _, dist in map(str.split, input.splitlines()):
    graph.setdefault(src, {})[dst] = int(dist)
    graph.setdefault(dst, {})[src] = int(dist)


print(min(sum(graph[a][b] for a, b in zip(locs, locs[1:])) for locs in itertools.permutations(graph)))
print(max(sum(graph[a][b] for a, b in zip(locs, locs[1:])) for locs in itertools.permutations(graph)))
