from lib import *

input = read_input(2021, 24)

lines = input.splitlines()


stack = []
out = [9] * 14
for i in range(14):
    x = int(lines[i * 18 + 5].split()[2])
    y = int(lines[i * 18 + 15].split()[2])
    if x < 0:
        j, y = stack.pop()
        x += y
        if x < 0:
            out[i] += x
        else:
            out[j] -= x
    else:
        stack.append((i, y))

print(int("".join(map(str, out))))


stack = []
out = [1] * 14
for i in range(14):
    x = int(lines[i * 18 + 5].split()[2])
    y = int(lines[i * 18 + 15].split()[2])
    if x < 0:
        j, y = stack.pop()
        x += y
        if x < 0:
            out[j] -= x
        else:
            out[i] += x
    else:
        stack.append((i, y))

print(int("".join(map(str, out))))
