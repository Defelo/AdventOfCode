from lib import *

input = read_input(2020, 3)

lines = input.splitlines()


x = 0
out = 0
for i, line in enumerate(lines):
    out += line[x] == "#"
    x = (x + 3) % len(line)

print(out)


out = 0
for i, line in enumerate(lines[::2]):
    out += line[i % len(line)] == "#"

for d in [1, 3, 5, 7]:
    x = 0
    o = 0
    for i, line in enumerate(lines):
        o += line[x] == "#"
        x = (x + d) % len(line)
    out *= o

print(out)
