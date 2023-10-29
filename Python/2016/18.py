from lib import *

input = read_input(2016, 18).strip()


def next_row(row):
    return "".join(".^"[a != b] for a, b in zip("." + row[:-1], row[1:] + "."))


row = input
out = 0
for _ in range(40):
    out += row.count(".")
    row = next_row(row)
print(out)


def next_row(row):
    return "".join(".^"[a != b] for a, b in zip("." + row[:-1], row[1:] + "."))


row = input
out = 0
for _ in range(400000):
    out += row.count(".")
    row = next_row(row)
print(out)
