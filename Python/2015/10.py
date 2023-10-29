from lib import *

input = read_input(2015, 10)


def las(inp):
    out = ""
    i = 0
    while i < len(inp):
        c = inp[i]
        j = 1
        while i + j < len(inp) and inp[i + j] == c:
            j += 1
        out += f"{j}{c}"
        i += j
    return out


x = input.strip()
for _ in range(40):
    x = las(x)
print(len(x))

x = input.strip()
for _ in range(50):
    x = las(x)
print(len(x))
