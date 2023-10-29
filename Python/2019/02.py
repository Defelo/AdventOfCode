from lib import *

input = read_input(2019, 2)

(*mem,) = map(int, input.strip().split(","))
i = 0
mem[1] = 12
mem[2] = 2
while i < len(mem):
    if mem[i] == 99:
        break
    elif mem[i] == 1:
        mem[mem[i + 3]] = mem[mem[i + 1]] + mem[mem[i + 2]]
    elif mem[i] == 2:
        mem[mem[i + 3]] = mem[mem[i + 1]] * mem[mem[i + 2]]
    i += 4
print(mem[0])
(*mem,) = map(int, input.strip().split(","))


def simulate(a, b, mem):
    i = 0
    mem[1] = a
    mem[2] = b
    while i < len(mem):
        if mem[i] == 99:
            break
        elif mem[i] == 1:
            mem[mem[i + 3]] = mem[mem[i + 1]] + mem[mem[i + 2]]
        elif mem[i] == 2:
            mem[mem[i + 3]] = mem[mem[i + 1]] * mem[mem[i + 2]]
        i += 4
    return mem[0]


print(*[a * 100 + b for a in range(100) for b in range(100) if simulate(a, b, mem[:]) == 19690720])
