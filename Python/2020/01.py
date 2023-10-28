from lib import *

input = read_input(2020, 1)

nums = ints(input)


seen = set()
for a in nums:
    b = 2020 - a
    if b in seen:
        print(a * b)
        break
    seen.add(a)


for i, a in enumerate(nums):
    seen = set()
    for b in nums[i + 1 :]:
        c = 2020 - a - b
        if c in seen:
            print(a * b * c)
            break
        seen.add(b)
    else:
        continue
    break
