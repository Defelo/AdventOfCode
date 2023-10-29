from lib import *

input = read_input(2016, 16).strip()


def fill(state, n):
    while len(state) < n:
        state = state + "0" + "".join("10"[c == "1"] for c in reversed(state))

    return state[:n]


def checksum(inp):
    if len(inp) % 2:
        return inp

    out = ""

    for i in range(0, len(inp), 2):
        out += "01"[inp[i] == inp[i + 1]]

    return checksum(out)


print(checksum(fill(input, 272)))
print(checksum(fill(input, 35651584)))
