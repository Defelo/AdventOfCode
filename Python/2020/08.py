from lib import *

input = read_input(2020, 8)

lines = input.splitlines()


acc = 0
pc = 0
seen = set()
while pc < len(lines):
    if pc in seen:
        break

    seen.add(pc)

    a, b = lines[pc].split()

    if a == "acc":
        acc += int(b)

        pc += 1

    elif a == "jmp":
        pc += int(b)

    elif a == "nop":
        pc += 1

print(acc)


def simulate(flip):
    acc = 0
    pc = 0
    seen = set()
    while pc < len(lines):
        if pc in seen:
            return None

        seen.add(pc)

        a, b = lines[pc].split()
        if pc == flip:
            if a == "nop":
                a = "jmp"

            elif a == "jmp":
                a = "nop"

        if a == "acc":
            acc += int(b)

            pc += 1

        elif a == "jmp":
            pc += int(b)

        elif a == "nop":
            pc += 1

    return acc


for i in range(len(lines)):
    if (out := simulate(i)) is not None:
        print(out)
        break
