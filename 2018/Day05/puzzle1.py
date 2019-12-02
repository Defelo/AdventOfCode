stack = []
for c in open("input.txt").read().strip():
    if stack and stack[-1].lower() == c.lower() and stack[-1].islower() != c.islower():
        stack.pop()
    else:
        stack.append(c)
print(len(stack))
