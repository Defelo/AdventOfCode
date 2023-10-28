from lib import *

input = read_input(2020, 5)

lines = input.splitlines()


out = 0
for line in lines:
    row = int(line[:7].replace("F", "0").replace("B", "1"), 2)
    col = int(line[-3:].replace("L", "0").replace("R", "1"), 2)
    out = max(out, row * 8 + col)

print(out)


seats = set()
found = set()
for line in lines:
    row = int(line[:7].replace("F", "0").replace("B", "1"), 2)
    col = int(line[-3:].replace("L", "0").replace("R", "1"), 2)
    found.add(s := row * 8 + col)
    seats |= {s - 1, s + 1}

print(next(iter(seats - found - {min(seats), max(seats)})))
