from lib import *

input = read_input(2018, 16)

opcodes = [
    lambda reg, a, b: reg[a] + reg[b],  # addr
    lambda reg, a, b: reg[a] + b,  # addi
    lambda reg, a, b: reg[a] * reg[b],  # mulr
    lambda reg, a, b: reg[a] * b,  # muli
    lambda reg, a, b: reg[a] & reg[b],  # banr
    lambda reg, a, b: reg[a] & b,  # bani
    lambda reg, a, b: reg[a] | reg[b],  # borr
    lambda reg, a, b: reg[a] | b,  # bori
    lambda reg, a, b: reg[a],  # setr
    lambda reg, a, b: a,  # seti
    lambda reg, a, b: a > reg[b],  # gtir
    lambda reg, a, b: reg[a] > b,  # gtri
    lambda reg, a, b: reg[a] > reg[b],  # gtrr
    lambda reg, a, b: a == reg[b],  # eqir
    lambda reg, a, b: reg[a] == b,  # eqri
    lambda reg, a, b: reg[a] == reg[b],  # eqrr
]


def exec_opcode(reg, op, a, b, c):
    reg[c] = int(op(reg, a, b))


def test_opcode(before, after, op, a, b, c):
    before = [*before]

    exec_opcode(before, op, a, b, c)

    return before == after


out = 0
for before, instruction, after in map(str.splitlines, input.split("\n\n\n")[0].split("\n\n")):
    before = eval(before.split(": ")[1])
    _, a, b, c = map(int, instruction.split())
    after = eval(after.split(": ")[1])
    out += sum(test_opcode(before, after, op, a, b, c) for op in opcodes) >= 3

print(out)


codes = [opcodes.copy() for _ in range(16)]
m = [None] * 16
for before, instruction, after in map(str.splitlines, input.split("\n\n\n")[0].split("\n\n")):
    before = eval(before.split(": ")[1])
    op, a, b, c = map(int, instruction.split())
    after = eval(after.split(": ")[1])
    for o in [*codes[op]]:
        if not test_opcode(before, after, o, a, b, c):
            codes[op].remove(o)
            if len(codes[op]) == 1:
                m[op] = codes[op][0]

q = [x for x in m if x]
while q:
    x = q.pop()
    for i, lst in enumerate(codes):
        if x in lst:
            lst.remove(x)
            if len(lst) == 1:
                m[i] = lst[0]
                q.append(m[i])

reg = [0] * 4

for line in input.split("\n\n\n\n")[1].splitlines():
    op, a, b, c = map(int, line.split())
    exec_opcode(reg, m[op], a, b, c)

print(reg[0])
