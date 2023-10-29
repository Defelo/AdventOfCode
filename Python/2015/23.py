from lib import *

input = read_input(2015, 23)

program = input.splitlines()

pc = 0
registers = {"a": 0, "b": 0}
while pc < len(program):
    cmd, args = program[pc].split(maxsplit=1)
    args = args.split(", ")
    if cmd == "hlf":
        registers[args[0]] //= 2
        pc += 1
    elif cmd == "tpl":
        registers[args[0]] *= 3
        pc += 1
    elif cmd == "inc":
        registers[args[0]] += 1
        pc += 1
    elif cmd == "jmp":
        pc += int(args[0])
    elif cmd == "jie":
        pc += int(args[1]) if registers[args[0]] % 2 == 0 else 1
    elif cmd == "jio":
        pc += int(args[1]) if registers[args[0]] == 1 else 1
    else:
        break

print(registers["b"])


pc = 0
registers = {"a": 1, "b": 0}
while pc < len(program):
    cmd, args = program[pc].split(maxsplit=1)
    args = args.split(", ")
    if cmd == "hlf":
        registers[args[0]] //= 2
        pc += 1
    elif cmd == "tpl":
        registers[args[0]] *= 3
        pc += 1
    elif cmd == "inc":
        registers[args[0]] += 1
        pc += 1
    elif cmd == "jmp":
        pc += int(args[0])
    elif cmd == "jie":
        pc += int(args[1]) if registers[args[0]] % 2 == 0 else 1
    elif cmd == "jio":
        pc += int(args[1]) if registers[args[0]] == 1 else 1
    else:
        break

print(registers["b"])
