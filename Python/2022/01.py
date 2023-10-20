from lib import *

input = read_input(2022, 1)

nums = [sum(ints(p)) for p in input.split("\n\n")]

print(max(nums))
print(sum(sorted(nums, reverse=True)[:3]))
