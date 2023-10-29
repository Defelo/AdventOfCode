from lib import *

input = read_input(2015, 24)

packages = list(map(int, input.splitlines()))

gs = sum(packages) // 3


dp = {}


def test(i, x, pkg):
    if x == 0:
        return [[]]
    if i == len(pkg):
        return []
    if (i, x) not in dp:
        out = [[0] + y for y in test(i + 1, x, pkg)]
        if packages[i] <= x:
            out += [[1] + y for y in test(i + 1, x - pkg[i], pkg)]
        dp[(i, x)] = out
    return dp[(i, x)]


arr = []
for x in test(0, gs, packages):
    f = [z for y, z in zip(x, packages) if y]
    if not test(0, gs, f):
        continue
    p = 1
    for y in f:
        p *= y
    arr.append((len(f), p))

print(min(arr)[1])


packages = list(map(int, input.splitlines()))
gs = sum(packages) // 4
dp = {}


def test(i, x, pkg):
    if x == 0:
        return [[]]
    if i == len(pkg):
        return []
    if (i, x) not in dp:
        out = [[0] + y for y in test(i + 1, x, pkg)]
        if packages[i] <= x:
            out += [[1] + y for y in test(i + 1, x - pkg[i], pkg)]
        dp[(i, x)] = out
    return dp[(i, x)]


arr = []
for x in test(0, gs, packages):
    f = [z for y, z in zip(x, packages) if y]
    if not test(0, gs, f):
        continue
    p = 1
    for y in f:
        p *= y
    arr.append((len(f), p))

print(min(arr)[1])
