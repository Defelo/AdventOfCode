import math
import re

moons = []
for line in open("input.txt").read().splitlines():
    x, y, z = map(int, re.match(r"^<x=([\-\d]+), y=([\-\d]+), z=([\-\d]+)>$", line).groups())
    moons.append(([x, y, z], [0, 0, 0]))

def make_state(dim):
    return tuple(e[dim] for moon in moons for e in moon)

def simulate_dimension(dim):
    history = {}
    time = 0
    while True:
        state = make_state(dim)
        if state in history:
            return time - history[state]
        history[state] = time
        time += 1

        for pos, vel in moons:
            for p2, _ in moons:
                if pos[dim] < p2[dim]:
                    vel[dim] += 1
                elif pos[dim] > p2[dim]:
                    vel[dim] -= 1
        for pos, vel in moons:
            pos[dim] += vel[dim]

x, y, z = map(simulate_dimension, range(3))
out = x * y // math.gcd(x, y)
out = out * z // math.gcd(out, z)
print(out)
