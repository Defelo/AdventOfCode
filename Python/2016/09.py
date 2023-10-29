from lib import *

input = read_input(2016, 9).strip()

out = 0
i = 0
while i < len(input):
    if input[i] == "(":
        x = input[i + 1 :].split(")")[0]
        a, b = map(int, x.split("x"))
        i += len(x) + 2
        out += b * min(a, len(input) - i)
        i += a
        continue
    out += 1
    i += 1

print(out)


def solve(inp):
    out = 0
    i = 0
    while i < len(inp):
        if inp[i] == "(":
            x = inp[i + 1 :].split(")")[0]
            a, b = map(int, x.split("x"))
            i += len(x) + 2
            out += b * solve(inp[i : i + a])
            i += a
            continue
        out += 1
        i += 1
    return out


print(solve(input))
