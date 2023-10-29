from lib import *

input = read_input(2016, 12)

program = input.splitlines()

registers = {k: 0 for k in "abcd"}


def simulate():
    pc = 0
    while pc < len(program):
        cmd, *args = program[pc].split()
        x = registers[args[0]] if args[0] in registers else int(args[0])
        if cmd == "cpy":
            registers[args[1]] = x
            pc += 1
        elif cmd == "inc":
            registers[args[0]] += 1
            pc += 1
        elif cmd == "dec":
            registers[args[0]] -= 1
            pc += 1
        elif cmd == "jnz":
            if x:
                pc += int(args[1])
            else:
                pc += 1
    return registers["a"]


print(simulate())


registers = {k: 0 for k in "abcd"}
registers["c"] = 1
print(simulate())
