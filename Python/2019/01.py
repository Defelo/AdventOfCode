from lib import *

input = read_input(2019, 1)

out = 0
for line in input.splitlines():
    out += int(line) // 3 - 2
print(out)


out = 0
for line in input.splitlines():
    fuel = int(line) // 3 - 2
    while fuel > 0:
        out += fuel
        fuel = fuel // 3 - 2
print(out)
