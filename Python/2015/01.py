from lib import *

input = read_input(2015, 1)

x = 0
cum = [(x := x + [-1, 1][c == "("]) for c in input]

print(cum[-1])
print(cum.index(-1) + 1)
