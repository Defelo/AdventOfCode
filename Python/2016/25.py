from lib import *

input = read_input(2016, 25)

program = input.splitlines()


def simulate(a):
    registers = {k: 0 for k in "abcd"}
    registers["a"] = a
    pc = 0
    while pc < len(program):
        cmd, x, y, *_ = program[pc].split() + [None]
        read = lambda a: registers[a] if a in registers else int(a)
        if cmd == "cpy":
            if y in registers:
                registers[y] = read(x)
            pc += 1
        elif cmd == "inc":
            if x in registers:
                registers[x] += 1
            pc += 1
        elif cmd == "dec":
            if x in registers:
                registers[x] -= 1
            pc += 1
        elif cmd == "jnz":
            if read(x) and y is not None:
                pc += read(y) or 1
            else:
                pc += 1
        elif cmd == "out":
            yield read(x)
            pc += 1


def test(a):
    for i, x in enumerate(simulate(a)):
        if i % 2 != x:
            return False

        if i > 100:
            return True

    return False


a = 0
while not test(a):
    a += 1

print(a)
