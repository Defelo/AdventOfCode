from lib import *

input = read_input(2020, 13)

lines = input.splitlines()

target = int(lines[0])
nums = [int(n) for n in lines[1].split(",") if n != "x"]
t, n = min([((target // n + 1) * n - target, n) for n in nums])
print(t * n)


nums = lines[1].split(",")
n = []
a = []
for i, x in enumerate(nums):
    if x == "x":
        continue
    x = int(x)
    n.append(x)
    a.append(x - i)

print(chinese_remainder(n, a))
