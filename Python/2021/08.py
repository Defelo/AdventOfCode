from lib import *

input = read_input(2021, 8)


out = 0
for line in input.splitlines():
    _, b = map(str.split, line.split(" | "))
    out += sum(len(x) in [2, 4, 3, 7] for x in b)
print(out)


SEGMENTS = {k: v for v, k in enumerate([119, 18, 93, 91, 58, 107, 111, 82, 127, 123])}


def get(p, num):
    return SEGMENTS.get(sum(("abcdefg"[p[i]] in num) << i for i in range(7)))


out = 0
for line in input.splitlines():
    a, b = map(str.split, line.split(" | "))
    for perm in itertools.permutations(range(7)):
        if all(get(perm, x) is not None for x in a):
            out += int("".join(str(get(perm, x)) for x in b))
            break

print(out)
