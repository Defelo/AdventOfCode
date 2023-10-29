from lib import *

input = read_input(2018, 2)


def count(box, n):
    return any(box.count(c) == n for c in set(box))


counter = {2: 0, 3: 0}
for line in input.splitlines():
    counter[2] += count(line, 2)
    counter[3] += count(line, 3)

print(counter[2] * counter[3])


def compare(a, b):
    return len(a) == len(b) and sum(x != y for x, y in zip(a, b)) == 1


lines = input.splitlines()
for a in lines:
    for b in lines:
        if compare(a, b):
            print("".join(x for x, y in zip(a, b) if x == y))
            break
    else:
        continue
    break
