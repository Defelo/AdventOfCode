from lib import *

input = read_input(2018, 5)

stack = []
for c in input.strip():
    if stack and stack[-1].lower() == c.lower() and stack[-1].islower() != c.islower():
        stack.pop()
    else:
        stack.append(c)
print(len(stack))


def react(poly, wo):
    stack = []
    for c in poly:
        if c.lower() == wo.lower():
            continue
        if stack and stack[-1].lower() == c.lower() and stack[-1].islower() != c.islower():
            stack.pop()
        else:
            stack.append(c)
    return len(stack)


inp = input.strip()
print(min(react(inp, c) for c in set(inp.lower())))
