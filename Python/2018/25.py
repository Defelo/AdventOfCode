from lib import *

input = read_input(2018, 25)


coords = [tuple(map(int, line.split(","))) for line in input.splitlines()]
uf = UnionFind(len(coords))
for i in range(len(coords)):
    for j in range(i + 1, len(coords)):
        if sum(abs(a - b) for a, b in zip(coords[i], coords[j])) <= 3:
            uf.merge(i, j)

print(len(set(map(uf.find, range(len(coords))))))
