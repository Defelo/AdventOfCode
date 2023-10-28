from lib import *

input = read_input(2020, 16)


def valid(rule, num):
    ranges = rule.split(": ")[1].split(" or ")

    for r in ranges:
        x, y = map(int, r.split("-"))

        if x <= num <= y:
            return True

    return False


rules, mt, nt = map(str.splitlines, input.split("\n\n"))
out = 0
nt.pop(0)
for ticket in nt:
    for f in map(int, ticket.split(",")):
        for r in rules:
            if valid(r, f):
                break
        else:
            out += f

print(out)


rules, mt, nt = map(str.splitlines, input.split("\n\n"))
mt = list(map(int, mt[1].split(",")))
nt = [list(map(int, ticket.split(","))) for ticket in nt[1:]]
nt = [ticket for ticket in nt if all(any(valid(r, f) for r in rules) for f in ticket)]
possible_allocations = [set(range(len(rules))) for i in range(len(rules))]
for ticket in nt:
    for field in range(len(rules)):
        for rule in range(len(rules)):
            if not valid(rules[rule], ticket[field]):
                possible_allocations[rule].remove(field)

allocations = [None] * len(rules)
while any(x is None for x in allocations):
    for i in range(len(rules)):
        if len(possible_allocations[i]) == 1:
            j = possible_allocations[i].pop()
            allocations[i] = j
            for k in range(len(rules)):
                if j in possible_allocations[k]:
                    possible_allocations[k].remove(j)

out = 1
for i, rule in enumerate(rules):
    if not rule.startswith("departure"):
        continue
    j = allocations[i]
    out *= mt[j]

print(out)
