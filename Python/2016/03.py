from lib import *

input = read_input(2016, 3)

print(sum((y := sorted(map(int, x)))[2] < y[0] + y[1] for x in map(str.split, input.splitlines())))


triangles = [int(b) for a in zip(*map(str.split, input.splitlines())) for b in a]
out = 0
while triangles:
    a, b, c = sorted([triangles.pop(0) for _ in "xxx"])
    out += c < a + b
print(out)
