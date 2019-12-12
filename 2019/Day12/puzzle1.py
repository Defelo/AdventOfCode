import re

moons = []
for line in open("input.txt").read().splitlines():
    x, y, z = map(int, re.match(r"^<x=([\-\d]+), y=([\-\d]+), z=([\-\d]+)>$", line).groups())
    moons.append(([x, y, z], [0, 0, 0]))

for _ in range(1000):
    for pos, vel in moons:
        for p2, _ in moons:
            for i in range(3):
                if pos[i] < p2[i]:
                    vel[i] += 1
                elif pos[i] > p2[i]:
                    vel[i] -= 1
    for pos, vel in moons:
        for i in range(3):
            pos[i] += vel[i]
print(sum(sum(map(abs, pos)) * sum(map(abs, vel)) for pos, vel in moons))
