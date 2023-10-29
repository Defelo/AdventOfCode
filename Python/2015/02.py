from lib import *

input = read_input(2015, 2)

boxes = [sorted(map(int, line.split("x"))) for line in input.splitlines()]

total = 0
for a, b, c in boxes:
    total += 2 * (a * b + a * c + b * c) + a * b
print(total)

total = 0
for a, b, c in boxes:
    total += 2 * (a + b) + a * b * c
print(total)
