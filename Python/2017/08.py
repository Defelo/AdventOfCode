from lib import *

input = read_input(2017, 8)

lines = input.splitlines()


registers = {}
for line in lines:
    reg, op, n, _, a, b, c = line.split()
    if eval(str(registers.get(a, 0)) + b + c):
        x = registers.get(reg, 0)
        registers[reg] = x + (-1 if op == "dec" else 1) * int(n)

print(max(registers.values()))


registers = {}
out = 0
for line in lines:
    reg, op, n, _, a, b, c = line.split()
    if eval(str(registers.get(a, 0)) + b + c):
        x = registers.get(reg, 0)
        registers[reg] = x + (-1 if op == "dec" else 1) * int(n)
        out = max(out, registers[reg])

print(out)
