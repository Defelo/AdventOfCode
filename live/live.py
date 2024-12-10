from lib import *

input = open(0).read().strip()

blocks = input.split("\n\n")
lines = input.splitlines()
nums = ints(input)

out = 0
for line in lines:
    match = re.match(r"^$", line)


print(out)
