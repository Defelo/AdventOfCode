import re

DECK = 10007
pos = 2019
for line in open("input.txt").read().splitlines():
    reverse = re.match(r"deal into new stack", line)
    cut = re.match(r"cut (-?\d+)", line)
    increment = re.match(r"deal with increment (\d+)", line)
    if reverse:
        pos = -pos - 1
    elif cut:
        pos -= int(cut.group(1))
    elif increment:
        pos *= int(increment.group(1))
    pos %= DECK

print(pos)
