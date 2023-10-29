from lib import *

input = read_input(2015, 19)

*replacements, _, mol = input.splitlines()
rep = {}
for k, _, v in map(str.split, replacements):
    rep.setdefault(k, []).append(v)

out = set()
for k, v in rep.items():
    for i in range(len(mol)):
        if k == mol[i : i + len(k)]:
            for e in v:
                out.add(mol[:i] + e + mol[i + len(k) :])

print(len(out))


*replacements, _, mol = input.splitlines()
rep = {}
for v, _, k in map(str.split, replacements):
    rep[k] = v


def replace(inp):
    out = set()
    for k, v in rep.items():
        for i in range(len(inp)):
            if k == inp[i : i + len(k)]:
                out.add(inp[:i] + v + inp[i + len(k) :])
    return out


def solve(inp):
    if inp == "e":
        return 0
    for x in replace(inp):
        if (y := solve(x)) is not None:
            return y + 1


print(solve(mol))
