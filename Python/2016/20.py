from lib import *

input = read_input(2016, 20)

blocked = []

for line in input.splitlines():
    a, b = map(int, line.split("-"))
    blocked.append((a, b))


is_blocked = lambda x: any(a <= x <= b for a, b in blocked)


for b in sorted(b for _, b in blocked):
    if not is_blocked(b + 1):
        print(b + 1)
        break


blocked = []
for line in input.splitlines():
    a, b = map(int, line.split("-"))

    merged = []

    for i, (x, y) in enumerate(blocked):
        if x <= a <= b <= y:
            break

        if y + 1 < a or b < x - 1:
            continue

        a, b = min(a, x), max(b, y)

        merged.append(i)

    else:
        blocked.append((a, b))

        for i in reversed(merged):
            blocked.pop(i)

print((1 << 32) - sum(b - a + 1 for a, b in blocked))
