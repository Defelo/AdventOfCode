from lib import *

input = read_input(2017, 6)


nums = [*map(int, input.split())]
seen = set()
out = 0
while tuple(nums) not in seen:
    seen.add(tuple(nums))
    idx = nums.index(max(nums))
    n = nums[idx]
    nums[idx] = 0
    i = idx
    for _ in range(n):
        i = (i + 1) % len(nums)
        nums[i] += 1
    out += 1

print(out)


nums = [*map(int, input.split())]
seen = {}
out = 0
while tuple(nums) not in seen:
    seen[tuple(nums)] = out
    idx = nums.index(max(nums))
    n = nums[idx]
    nums[idx] = 0
    i = idx
    for _ in range(n):
        i = (i + 1) % len(nums)
        nums[i] += 1
    out += 1

print(out - seen[tuple(nums)])
