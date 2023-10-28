from lib import *

input = read_input(2021, 21)

lines = input.splitlines()


players = [int(line.split()[-1]) for line in lines]
scores = [0, 0]
die = 0
k = 0
while max(scores) < 1000:
    x = sum((die + i) % 100 + 1 for i in range(3))
    die += 3
    players[k] = (players[k] - 1 + x) % 10 + 1
    scores[k] += players[k]
    k = (k + 1) % 2

print(scores[k] * die)


@functools.cache
def dirac(p1, p2, s1, s2, k):
    if s1 >= 21:
        return 1, 0

    if s2 >= 21:
        return 0, 1

    p = [p1, p2][k]
    s = [s1, s2][k]
    out = [0, 0]
    for x in map(sum, itertools.product([1, 2, 3], repeat=3)):
        q = (p - 1 + x) % 10 + 1
        if k == 0:
            res = dirac(q, p2, s + q, s2, 1)
        else:
            res = dirac(p1, q, s1, s + q, 0)
        for i in range(2):
            out[i] += res[i]

    return tuple(out)


dirac.cache_clear()
players = [int(line.split()[-1]) for line in lines]
print(max(dirac(*players, 0, 0, 0)))
