from lib import *

input = read_input(2020, 15)


nums = ints(input)
hist = {n: i for i, n in enumerate(nums[:-1])}
n = nums[-1]
for turn in range(len(nums), 2020):
    o = turn - 1 - hist[n] if n in hist else 0
    hist[n] = turn - 1
    n = o

print(n)


hist = {n: i for i, n in enumerate(nums[:-1])}
n = nums[-1]
for turn in range(len(nums), 30000000):
    o = turn - 1 - hist[n] if n in hist else 0
    hist[n] = turn - 1
    n = o

print(n)
