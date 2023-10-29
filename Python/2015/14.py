from lib import *

input = read_input(2015, 14)

T = 2503
out = 0


for line in map(str.split, input.splitlines()):
    v, t, r = map(int, [line[3], line[6], line[-2]])
    x = T // (t + r) * t * v
    x += min(t, T % (t + r)) * v
    out = max(out, x)

print(out)


T = 2503
reindeers = [tuple(map(int, [line[3], line[6], line[-2]])) for line in map(str.split, input.splitlines())]
states = [0] * len(reindeers)
meters = [0] * len(reindeers)
bonus = [0] * len(reindeers)
for _ in range(T):
    for i, (v, t, r) in enumerate(reindeers):
        if states[i] >= 0:
            meters[i] += v

        states[i] += 1
        if states[i] >= t:
            states[i] = -r

    mx = max(meters)
    for i, m in enumerate(meters):
        if m == mx:
            bonus[i] += 1

print(max(bonus))
