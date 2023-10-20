from lib import *

input = read_input(2022, 4)

lines = [*map(pints, input.splitlines())]


print(sum(a <= c <= d <= b or c <= a <= b <= d for a, b, c, d in lines))
print(sum(d >= a and c <= b for a, b, c, d in lines))
