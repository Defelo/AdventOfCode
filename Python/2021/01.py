import sys

from lib import *

input = read_input(2021, 1)

nums = ints(input)

print(sum(b > a for a, b in sliding_window(nums, 2)))
print(sum(b > a for a, b in zip(nums, nums[3:])))
