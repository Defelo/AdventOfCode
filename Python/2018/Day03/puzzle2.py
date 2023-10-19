import re

claims = open("input.txt").read().splitlines()

G = [[0 for _ in range(1000)] for _ in range(1000)]
for line in claims:
    offx, offy, wid, hei = map(int, re.match("^#\d+ @ (\d+),(\d+): (\d+)x(\d+)$", line).groups())
    for x in range(wid):
        for y in range(hei):
            G[offx + x][offy + y] += 1
for line in claims:
    id, offx, offy, wid, hei = map(int, re.match("^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$", line).groups())
    if all(G[offx + x][offy + y] == 1 for x in range(wid) for y in range(hei)):
        print(id)
