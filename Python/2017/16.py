from lib import *

input = read_input(2017, 16).strip()


p = [chr(97 + i) for i in range(16)]
for cmd in input.split(","):
    if cmd[0] == "s":
        n = int(cmd[1:])
        p = p[-n:] + p[:-n]
    elif cmd[0] == "x":
        a, b = map(int, cmd[1:].split("/"))
        p[a], p[b] = p[b], p[a]
    elif cmd[0] == "p":
        a, b = map(p.index, cmd[1:].split("/"))
        p[a], p[b] = p[b], p[a]
print("".join(p))


p = [chr(97 + i) for i in range(16)]
q = p[:]
mem = []
while True:
    mem.append(p[:])
    for cmd in input.split(","):
        if cmd[0] == "s":
            n = int(cmd[1:])
            p = p[-n:] + p[:-n]
        elif cmd[0] == "x":
            a, b = map(int, cmd[1:].split("/"))
            p[a], p[b] = p[b], p[a]
        elif cmd[0] == "p":
            a, b = map(p.index, cmd[1:].split("/"))
            p[a], p[b] = p[b], p[a]
    if p == q:
        break

print("".join(mem[1000000000 % len(mem)]))
