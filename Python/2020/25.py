from lib import *

input = read_input(2020, 25)

lines = input.splitlines()


(*nums,) = map(int, lines)
k = 1
i = 0
while k not in nums:
    k = (k * 7) % 20201227
    i += 1

print(pow(nums[1 - nums.index(k)], i, 20201227))
