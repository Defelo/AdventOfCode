from lib import *

input = read_input(2017, 21)

lines = input.splitlines()


def rotate(p):
    return tuple("".join(p[j][i] for j in range(len(p))) for i in reversed(range(len(p[0]))))


rules = {}
pattern = ".#.", "..#", "###"
for line in lines:
    a, b = [tuple(k.split("/")) for k in line.split(" => ")]
    rules[a] = b


def find_rule(p):
    for _ in range(4):
        if p in rules:
            return rules[p]
        p = rotate(p)

    p = tuple(reversed(p))
    for _ in range(4):
        if p in rules:
            return rules[p]
        p = rotate(p)


def apply():
    k = 3 if len(pattern) % 2 else 2
    out = []
    for i in range(len(pattern) // k):
        b = []
        for j in range(len(pattern) // k):
            new = find_rule(tuple("".join(pattern[i * k + y][j * k + x] for x in range(k)) for y in range(k)))
            b.append(new)
        for line in zip(*b):
            out.append("".join(line))
    return tuple(out)


for _ in range(5):
    pattern = apply()

print(sum(line.count("#") for line in pattern))


rules = {}
pattern = ".#.", "..#", "###"
for line in lines:
    a, b = [tuple(k.split("/")) for k in line.split(" => ")]
    rules[a] = b


def find_rule(p):
    for _ in range(4):
        if p in rules:
            return rules[p]
        p = rotate(p)
    p = tuple(reversed(p))
    for _ in range(4):
        if p in rules:
            return rules[p]
        p = rotate(p)


def apply():
    k = 3 if len(pattern) % 2 else 2
    out = []
    for i in range(len(pattern) // k):
        b = []
        for j in range(len(pattern) // k):
            new = find_rule(tuple("".join(pattern[i * k + y][j * k + x] for x in range(k)) for y in range(k)))
            b.append(new)
        for line in zip(*b):
            out.append("".join(line))
    return tuple(out)


for _ in range(18):
    pattern = apply()

print(sum(line.count("#") for line in pattern))
