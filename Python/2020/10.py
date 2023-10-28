from lib import *

input = read_input(2020, 10)

from collections import Counter

nums = sorted(ints(input))


prev = 0
counter = Counter()
for x in nums:
    d = x - prev
    counter[d] += 1
    prev = x

counter[3] += 1
print(counter[1] * counter[3])


dp = {}


def count(idx, joltage):
    if idx == len(nums):
        return joltage == nums[-1]

    if nums[idx] - joltage > 3:
        return 0

    if (idx, joltage) not in dp:
        dp[(idx, joltage)] = count(idx + 1, joltage) + count(idx + 1, nums[idx])

    return dp[(idx, joltage)]


print(count(0, 0))
