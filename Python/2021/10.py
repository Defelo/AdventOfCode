from lib import *

input = read_input(2021, 10)

lines = input.splitlines()

out = 0
for line in lines:
    stack = []

    for c in line:
        if c in "([{<":
            stack.append(c)

        else:
            x = stack.pop()

            if x + c not in ["()", "[]", "{}", "<>"]:
                out += {")": 3, "]": 57, "}": 1197, ">": 25137}[c]

                break

    else:
        out += 0


print(out)


out = []
for line in lines:
    stack = []

    for c in line:
        if c in "([{<":
            stack.append(c)

        else:
            x = stack.pop()

            if x + c not in ["()", "[]", "{}", "<>"]:
                break

    else:
        s = 0

        for c in reversed(stack):
            s = s * 5 + " ([{<".index(c)

        out.append(s)

out.sort()
print(out[len(out) // 2])
