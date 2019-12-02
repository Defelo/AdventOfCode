import re

G = [[0 for _ in range(1000)] for _ in range(1000)]
for line in open("input.txt").read().splitlines():
    offx, offy, wid, hei = map(int, re.match("^#\d+ @ (\d+),(\d+): (\d+)x(\d+)$", line).groups())
    for x in range(wid):
        for y in range(hei):
            G[offx + x][offy + y] += 1
print(sum(x>=2 for row in G for x in row))
