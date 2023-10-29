from lib import *

input = read_input(2015, 11)


def inc(x):
    x = [ord(c) - 0x61 for c in x]
    for i in range(len(x))[::-1]:
        x[i] += 1
        while chr(0x61 + x[i]) in "iol":
            x[i] += 1
        if x[i] < 26:
            break
        x[i] = 0
    return "".join(chr(c + 0x61) for c in x)


def check(x):
    return (
        all(c not in x for c in "iol")
        and any((chr(i) + chr(i + 1) + chr(i + 2)) in x for i in range(0x61, 0x61 + 24))
        and sum(chr(0x61 + i) * 2 in x for i in range(26)) >= 2
    )


def nxt(x):
    x = inc(x)
    while not check(x):
        x = inc(x)
    return x


print(a := nxt(input.strip()))
print(nxt(a))
