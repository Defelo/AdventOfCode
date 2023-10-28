from lib import *

input = read_input(2020, 6)


out = 0
for group in input.split("\n\n"):
    out += len(set(group.replace("\n", "")))

print(out)


out = 0
for group in input.split("\n\n"):
    x = set(group.replace("\n", ""))
    for person in group.split():
        x &= set(person)
    out += len(x)

print(out)
