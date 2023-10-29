from lib import *

input = read_input(2017, 4)

lines = input.splitlines()


out = 0
for pw in lines:
    x = pw.split()
    out += len(set(x)) == len(x)
print(out)


out = 0
for pw in lines:
    x = ["".join(sorted(e)) for e in pw.split()]
    out += len(set(x)) == len(x)
print(out)
