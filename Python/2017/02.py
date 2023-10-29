from lib import *

input = read_input(2017, 2)

lines = input.splitlines()


s = 0
for line in lines:
    nums = [*map(int, line.split())]
    s += max(nums) - min(nums)
print(s)


s = 0
for line in lines:
    nums = [*map(int, line.split())]
    for a in nums:
        for b in nums:
            if a % b == 0 and a != b:
                s += a // b
                break
        else:
            continue
        break

print(s)
