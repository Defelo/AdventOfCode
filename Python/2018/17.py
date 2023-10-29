import sys

from lib import *

sys.setrecursionlimit(10000)

input = read_input(2018, 17)

lines = input.splitlines()


clay = set()
miny = 1e1337
maxy = 0
for line in lines:
    a, b = line.split(", ")
    a = int(a.split("=")[1])
    b1, b2 = map(int, b.split("=")[1].split(".."))
    for i in range(b1, b2 + 1):
        if line[0] == "x":
            x, y = a, i
        else:
            x, y = i, a
        clay.add((x, y))
        miny = min(y, miny)
        maxy = max(y, maxy)

reachable = set()
water = set()
dp = {}


def flow(x, y):
    if y > maxy:
        return False
    if (x, y) in clay:
        return True
    if (x, y) in dp:
        return dp[(x, y)]
    reachable.add((x, y))

    if not flow(x, y + 1):
        dp[(x, y)] = False
        return False

    add = set()
    ok = True
    k = x
    while (k, y) not in clay:
        add.add((k, y))
        if not flow(k, y + 1):
            ok = False
            break
        k -= 1

    k = x
    while (k, y) not in clay:
        add.add((k, y))
        if not flow(k, y + 1):
            ok = False
            break
        k += 1

    reachable.update(add)
    if ok:
        water.update(add)
    dp[(x, y)] = ok
    return ok


flow(500, 0)

print(sum((x, y) != (500, 0) and y in range(miny, maxy + 1) for x, y in reachable))


clay = set()
miny = 1e1337
maxy = 0
for line in lines:
    a, b = line.split(", ")
    a = int(a.split("=")[1])
    b1, b2 = map(int, b.split("=")[1].split(".."))
    for i in range(b1, b2 + 1):
        if line[0] == "x":
            x, y = a, i
        else:
            x, y = i, a
        clay.add((x, y))
        miny = min(y, miny)
        maxy = max(y, maxy)

reachable = set()
water = set()
dp = {}


def flow(x, y):
    if y > maxy:
        return False
    if (x, y) in clay:
        return True
    if (x, y) in dp:
        return dp[(x, y)]

    reachable.add((x, y))
    if not flow(x, y + 1):
        dp[(x, y)] = False
        return False

    add = set()
    ok = True
    k = x
    while (k, y) not in clay:
        add.add((k, y))
        if not flow(k, y + 1):
            ok = False
            break
        k -= 1

    k = x
    while (k, y) not in clay:
        add.add((k, y))
        if not flow(k, y + 1):
            ok = False
            break

        k += 1
    reachable.update(add)
    if ok:
        water.update(add)

    dp[(x, y)] = ok
    return ok


flow(500, 0)

print(len(water))
