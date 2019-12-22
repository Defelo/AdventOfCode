import re

DECK = 119315717514047
REP = 101741582076661
first, inc = 0, 1

instructions = open("input.txt").read().splitlines()
for line in instructions:
    reverse = re.match(r"deal into new stack", line)
    cut = re.match(r"cut (-?\d+)", line)
    increment = re.match(r"deal with increment (\d+)", line)
    if reverse:
        add, mul = -1, -1
    elif cut:
        add, mul = int(cut.group(1)), 1
    elif increment:
        add, mul = 0, pow(int(increment.group(1)), DECK - 2, DECK)
    first, inc = first + add * inc, inc * mul
first, inc = first * (1 - pow(inc, REP, DECK)) * pow(1 - inc, DECK - 2, DECK), pow(inc, REP, DECK)

print((2020 * inc + first) % DECK)
