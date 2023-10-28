from lib import *

input = read_input(2020, 23)


def move1(state):
    current, moving, suffix = state[0], state[1:4], state[4:]
    dst = suffix.index(max([x for x in suffix if int(x) < int(current)], default=max(suffix)))
    state = current + suffix[: dst + 1] + moving + suffix[dst + 1 :]
    return state[1:] + state[0]


state = input.strip()
for _ in range(100):
    state = move1(state)
one = state.index("1")
print(state[one + 1 :] + state[:one])


def move2(state, current):
    first = state[current]
    second = state[first]
    third = state[second]
    suffix = state[third]
    dst = current
    while dst in (current, first, second, third):
        dst -= 1
        if not dst:
            dst = len(state) - 1

    state[current] = suffix
    state[third] = state[dst]
    state[dst] = first
    return state[current]


nums = list(map(int, input.strip()))
state = list(range(1, 1000002))
for a, b in zip([-1] + nums, nums + [len(nums) + 1]):
    state[a] = b

current = nums[0]
for _ in range(10000000):
    current = move2(state, current)
first = state[1]
second = state[first]
print(first * second)
