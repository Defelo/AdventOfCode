def count(box, n):
    return any(box.count(c) == n for c in set(box))

counter = {2: 0, 3: 0}
for line in open("input.txt").read().splitlines():
    counter[2] += count(line, 2)
    counter[3] += count(line, 3)

print(counter[2] * counter[3])
