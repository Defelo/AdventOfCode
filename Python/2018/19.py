from lib import *

input = read_input(2018, 19)

lines = input.splitlines()

opcodes = {
    "addr": lambda reg, a, b: reg[a] + reg[b],
    "addi": lambda reg, a, b: reg[a] + b,
    "mulr": lambda reg, a, b: reg[a] * reg[b],
    "muli": lambda reg, a, b: reg[a] * b,
    "banr": lambda reg, a, b: reg[a] & reg[b],
    "bani": lambda reg, a, b: reg[a] & b,
    "borr": lambda reg, a, b: reg[a] | reg[b],
    "bori": lambda reg, a, b: reg[a] | b,
    "setr": lambda reg, a, b: reg[a],
    "seti": lambda reg, a, b: a,
    "gtir": lambda reg, a, b: a > reg[b],
    "gtri": lambda reg, a, b: reg[a] > b,
    "gtrr": lambda reg, a, b: reg[a] > reg[b],
    "eqir": lambda reg, a, b: a == reg[b],
    "eqri": lambda reg, a, b: reg[a] == b,
    "eqrr": lambda reg, a, b: reg[a] == reg[b],
}


def exec_opcode(reg, op, a, b, c):
    reg[c] = int(opcodes[op](reg, a, b))


ip, *instructions = lines
ip = int(ip.split()[1])
reg = [0] * 6
while reg[ip] in range(len(instructions)):
    op, a, b, c = instructions[reg[ip]].split()
    exec_opcode(reg, op, *map(int, [a, b, c]))
    reg[ip] += 1

print(reg[0])


r1 = 2 * 2 * 19 * 11 + 2 * 22 + 12 + (27 * 28 + 29) * 30 * 14 * 32
r0 = 0
for r5 in range(1, r1 + 1):
    if r1 % r5 == 0:
        r0 += r5

print(r0)
