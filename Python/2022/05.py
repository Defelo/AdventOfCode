from lib import *

input = read_input(2022, 5)

initial, instructions = input.split("\n\n")
nums, *initial = initial.splitlines()[::-1]

stacks = [[] for _ in pints(nums)]
for line in initial:
    for i in range(0, len(line), 4):
        if line[i + 1] != " ":
            stacks[i // 4].append(line[i + 1])

instructions = [(cnt, i - 1, j - 1) for cnt, i, j in map(pints, instructions.splitlines())]


_stacks = deepcopy(stacks)
for cnt, i, j in instructions:
    for _ in range(cnt):
        stacks[j].append(stacks[i].pop())
print("".join(s[-1] for s in stacks))


stacks = _stacks
for cnt, i, j in instructions:
    stacks[j] += stacks[i][-cnt:]
    del stacks[i][-cnt:]
print("".join(s[-1] for s in stacks))
