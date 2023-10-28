from lib import *

input = read_input(2021, 2)

d = 0
h = 0
for line in input.splitlines():
    cmd, n = line.split()
    n = int(n)
    if cmd == "forward":
        h += n
    elif cmd == "up":
        d -= n
    elif cmd == "down":
        d += n

print(d * h)


a = 0
d = 0
h = 0
for line in input.splitlines():
    cmd, n = line.split()
    n = int(n)
    if cmd == "forward":
        h += n
        d += n * -a
    elif cmd == "up":
        a += n
    elif cmd == "down":
        a -= n

print(d * h)
