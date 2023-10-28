from lib import *

input = read_input(2020, 9)

nums = ints(input)


def test(i, num):
    seen = set()
    for j in range(25):
        x = nums[j + i]
        if num - x != x and num - x in seen:
            return True
        seen.add(x)
    return False


for i, x in enumerate(nums[25:]):
    if not test(i, x):
        target = x
        break

print(target)


for a in range(len(nums)):
    cum = 0
    for b in range(len(nums) - a):
        cum += nums[a + b]
        if b >= 1 and cum == target:
            x = nums[a : a + b + 1]
            print(min(x) + max(x))
            exit()
        elif cum > target:
            break
