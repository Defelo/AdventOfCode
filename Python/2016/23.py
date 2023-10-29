from lib import *

input = read_input(2016, 23)

program = input.splitlines()

registers = {k: 0 for k in "abcd"}


def simulate():
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

        elif cmd == "tgl":
            if pc + (x := read(x)) in range(len(program)):
                cmd, args = program[pc + x].split(maxsplit=1)

                if cmd == "inc":
                    cmd = "dec"

                elif cmd in ["dec", "tgl"]:
                    cmd = "inc"

                elif cmd == "jnz":
                    cmd = "cpy"

                elif cmd in ["cpy"]:
                    cmd = "jnz"

                program[pc + x] = f"{cmd} {args}"

            pc += 1

    return registers["a"]


registers["a"] = 7
print(simulate())


program = input.splitlines()
registers = {k: 0 for k in "abcd"}
registers["a"] = 7
print(math.factorial(12) + simulate() - math.factorial(7))
