from lib import *

input = read_input(2021, 14)

lines = input.splitlines()


def solve(n):
    adj = collections.Counter(sliding_window(lines[0]))
    rules = dict(line.split(" -> ") for line in lines[2:])
    for _ in range(n):
        new = collections.Counter()
        for (a, b), c in adj.items():
            if x := rules.get(a + b):
                new[(a, x)] += c
                new[(x, b)] += c
            else:
                new[(a, b)] += c
        adj = new

    chars = collections.Counter(lines[0][0])
    for (_, b), c in adj.items():
        chars[b] += c

    a, b = minmax(chars.values())
    return b - a


print(solve(10))
print(solve(40))
