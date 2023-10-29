from lib import *

input = read_input(2017, 23)

lines = input.splitlines()


registers = {chr(i): 0 for i in range(97, 105)}
pc = 0
out = 0
while pc in range(len(lines)):
    cmd, *args = lines[pc].split()
    get = lambda a: registers[a] if a in registers else int(a)
    if cmd == "set":
        registers[args[0]] = get(args[1])
    elif cmd == "sub":
        registers[args[0]] -= get(args[1])
    elif cmd == "mul":
        registers[args[0]] *= get(args[1])
        out += 1
    elif cmd == "jnz":
        if get(args[0]):
            pc += get(args[1])
            continue
    pc += 1

print(out)


b = 109900 - 17
h = 0
while b != 126900:
    b += 17
    f = 1
    for d in range(2, b):
        if b % d == 0:
            f = 0
            break
    if f == 0:
        h += 1

print(h)
