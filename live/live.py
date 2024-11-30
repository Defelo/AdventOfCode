from lib import *

input = read_input(2024, 1)

blocks = input.split("\n\n")
lines = input.splitlines()
nums = ints(input)

out = 0
for line in lines:
    match = re.match(r"^$", line)


ans(out)
