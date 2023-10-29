from lib import *

input = read_input(2017, 5)

lines = input.splitlines()


jumps = [*map(int, lines)]
pos = 0
i = 0
while pos in range(len(jumps)):
    p = pos
    pos += jumps[pos]
    jumps[p] += 1
    i += 1

print(i)


jumps = [*map(int, lines)]
pos = 0
i = 0
while pos in range(len(jumps)):
    p = pos
    pos += jumps[pos]
    if jumps[p] >= 3:
        jumps[p] -= 1
    else:
        jumps[p] += 1
    i += 1

print(i)
