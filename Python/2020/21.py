from lib import *

input = read_input(2020, 21)

lines = input.splitlines()


allergens = {}
ing = set()
cnt = Counter()
for line in lines:
    i, a = line.split(" (contains ")
    i = i.split()
    ing.update(i)
    cnt.update(i)
    for x in a.strip(")").split(", "):
        if x not in allergens:
            allergens[x] = set(i)
        else:
            allergens[x] &= set(i)

for k in allergens.values():
    ing -= k

print(sum(cnt[x] for x in ing))


allergens = {}
for line in lines:
    i, a = line.split(" (contains ")
    i = i.split()
    for x in a.strip(")").split(", "):
        if x not in allergens:
            allergens[x] = set(i)
        else:
            allergens[x] &= set(i)

found = {}
while allergens:
    for k, v in [*allergens.items()]:
        if len(v) != 1:
            continue
        (found[k],) = v
        for t in allergens.values():
            if found[k] in t:
                t.remove(found[k])
        allergens.pop(k)

print(",".join(b for a, b in sorted(found.items())))
