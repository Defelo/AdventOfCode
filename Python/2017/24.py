from lib import *

input = read_input(2017, 24)

lines = input.splitlines()


comps = sorted([tuple(sorted(map(int, line.split("/")))) for line in lines])
used = set()


def solve(port):
    best = 0
    for i, (p, q) in enumerate(comps):
        if i in used:
            continue
        if q == port:
            p, q = q, p
        if p != port:
            continue
        used.add(i)
        best = max(best, p + q + solve(q))
        used.remove(i)
    return best


print(solve(0))


comps = sorted([tuple(sorted(map(int, line.split("/")))) for line in lines])
used = set()


def solve(port):
    best = 0, 0
    for i, (p, q) in enumerate(comps):
        if i in used:
            continue
        if q == port:
            p, q = q, p
        if p != port:
            continue
        used.add(i)
        a, b = solve(q)
        best = max(best, (a + 1, p + q + b))
        used.remove(i)
    return best


print(solve(0)[1])
