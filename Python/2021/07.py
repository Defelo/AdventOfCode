from lib import *

input = read_input(2021, 7)


nums = ints(input)


def align1(pos):
    return sum(abs(n - pos) for n in nums)


print(min(map(align1, range(len(nums)))))


def align2(pos):
    return sum((x := abs(n - pos)) * (x + 1) // 2 for n in nums)


print(min(map(align2, range(len(nums)))))
