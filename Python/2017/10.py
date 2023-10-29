from lib import *

input = read_input(2017, 10).strip()


nums = [*range(256)]
pos = 0
skip = 0


def rev(a, b):
    a %= len(nums)
    b = (b - a) % len(nums)
    nums[:] = nums[a:] + nums[:a]
    nums[:b] = nums[:b][::-1]
    nums[:] = nums[-a:] + nums[:-a]


for length in map(int, input.split(",")):
    rev(pos, pos + length)
    pos += length + skip
    skip += 1

print(nums[0] * nums[1])


lengths = [*map(ord, input), 17, 31, 73, 47, 23]
nums = [*range(256)]
pos = 0
skip = 0


def rev(a, b):
    a %= len(nums)
    b = (b - a) % len(nums)
    nums[:] = nums[a:] + nums[:a]
    nums[:b] = nums[:b][::-1]
    nums[:] = nums[-a:] + nums[:-a]


for _ in range(64):
    for length in lengths:
        rev(pos, pos + length)
        pos += length + skip
        skip += 1

dense = []
for i in range(16):
    x = 0
    for j in range(16):
        x ^= nums[i * 16 + j]

    dense.append(x)

print("".join(f"{x:02x}" for x in dense))
