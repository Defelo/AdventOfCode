from lib import *

input = read_input(2020, 22)


p1, p2 = [[*map(int, x.splitlines()[1:])] for x in input.split("\n\n")]
while p1 and p2:
    if p1[0] > p2[0]:
        p1.append(p1.pop(0))
        p1.append(p2.pop(0))
    else:
        p2.append(p2.pop(0))
        p2.append(p1.pop(0))

print(sum((i + 1) * x for i, x in enumerate(reversed(p1 or p2))))


def combat(p1, p2):
    seen = set()
    while p1 and p2:
        if (k := (tuple(p1), tuple(p2))) in seen:
            return 0, None
        seen.add(k)
        c1 = p1.pop(0)
        c2 = p2.pop(0)
        if len(p1) >= c1 and len(p2) >= c2:
            winner, _ = combat(p1[:c1], p2[:c2])
        else:
            winner = c2 > c1
        if not winner:
            p1.append(c1)
            p1.append(c2)
        else:
            p2.append(c2)
            p2.append(c1)
    return bool(p2), p1 or p2


p1, p2 = [[*map(int, x.splitlines()[1:])] for x in input.split("\n\n")]
_, p = combat(p1, p2)
print(sum((i + 1) * x for i, x in enumerate(reversed(p))))
