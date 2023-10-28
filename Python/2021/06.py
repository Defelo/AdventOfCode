from lib import *

input = read_input(2021, 6)


nums = ints(input)
nums = [*map(nums.count, range(9))]
for _ in range(80):
    nums.append(x := nums.pop(0))
    nums[6] += x

print(sum(nums))


nums = ints(input)
nums = [*map(nums.count, range(9))]
for _ in range(256):
    nums.append(x := nums.pop(0))
    nums[6] += x

print(sum(nums))
