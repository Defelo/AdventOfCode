from lib import *

input = read_input(2017, 7)

lines = input.splitlines()


names = set()
subs = set()
for line in lines:
    name = line.split()[0]
    sub = [] if "->" not in line else line.split("-> ")[1].split(", ")
    names.add(name)
    subs.update(sub)

print(next(iter(names - subs)))


progs = {}
names = set()
subs = set()
for line in lines:
    name = line.split()[0]
    weight = int(line.split("(")[1].split(")")[0])
    sub = [] if "->" not in line else line.split("-> ")[1].split(", ")
    progs[name] = weight, sub
    names.add(name)
    subs.update(sub)

root = next(iter(names - subs))


def check_sum(p):
    s = progs[p][0]
    w = {}
    for q in progs[p][1]:
        a, b = check_sum(q)
        if a is not None:
            return a, None
        s += b
        w[q] = b

    x = max(w.values(), key=list(w.values()).count, default=None)

    for k, v in w.items():
        if v != x:
            return x - (v - progs[k][0]), None

    return None, s


print(check_sum(root)[0])
