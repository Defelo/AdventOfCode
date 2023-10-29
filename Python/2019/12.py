from lib import *

input = read_input(2019, 12)

moons = []
for line in input.splitlines():
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


moons = []
for line in input.splitlines():
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
