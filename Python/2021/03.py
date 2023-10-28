from lib import *

input = read_input(2021, 3)


a = ""
b = ""
for x in transpose(input.splitlines()):
    a += most_common(x, "01")
    b += least_common(x, "01")

print(int(a, 2) * int(b, 2))


def find(x):
    out = {*input.splitlines()}

    for i in range(len(input.splitlines()[0])):
        digits = transpose(out)[i]

        mx = [least_common, most_common][x](digits, f"{x}{1-x}")

        out = {n for n in out if n[i] == mx}

        if len(out) == 1:
            return int(next(iter(out)), 2)


print(find(0) * find(1))
