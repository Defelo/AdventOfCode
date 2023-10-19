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

inp = open("input.txt").read().strip()
print(min(react(inp, c) for c in set(inp.lower())))
