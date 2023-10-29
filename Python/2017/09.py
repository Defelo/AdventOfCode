from lib import *

input = read_input(2017, 9)


out = 0
level = 0
garbage = False
skip = False
for c in input:
    if skip:
        skip = False
        continue
    if garbage:
        if c == "!":
            skip = True
        elif c == ">":
            garbage = False
    else:
        if c == "{":
            level += 1
        elif c == "}":
            out += level
            level -= 1
        elif c == "<":
            garbage = True

print(out)


out = 0
level = 0
garbage = False
skip = False
for c in input:
    if skip:
        skip = False
        continue
    if garbage:
        if c == "!":
            skip = True
        elif c == ">":
            garbage = False
        else:
            out += 1
    else:
        if c == "{":
            level += 1
        elif c == "}":
            level -= 1
        elif c == "<":
            garbage = True

print(out)
