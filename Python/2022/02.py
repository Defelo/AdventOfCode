from lib import *

input = read_input(2022, 2)

part1 = 0
part2 = 0
for line in input.splitlines():
    a, b = line.split()
    a = ord(a) - ord("A")
    b = ord(b) - ord("X")

    w = (a - b) % 3
    if w == 0:
        part1 += 3 + b + 1
    elif w == 1:
        part1 += b + 1
    elif w == 2:
        part1 += 6 + b + 1

    w = (a + b - 1) % 3
    if b == 0:
        part2 += w + 1
    elif b == 1:
        part2 += 3 + w + 1
    elif b == 2:
        part2 += 6 + w + 1

print(part1)
print(part2)
