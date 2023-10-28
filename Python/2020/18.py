from lib import *

input = read_input(2020, 18)

lines = input.splitlines()


def evaluate1(expr):
    new_expr = ""
    cnt = 0
    buffer = ""
    for c in expr.replace(" ", ""):
        if c == "(":
            if cnt:
                buffer += c
            cnt += 1
        elif c == ")":
            cnt -= 1
            if cnt:
                buffer += c
            else:
                new_expr += str(evaluate1(buffer))
                buffer = ""
        elif cnt:
            buffer += c
        else:
            new_expr += c

    out = 0
    buffer = 0
    op = False
    for c in new_expr + " ":
        if c.isnumeric():
            buffer = buffer * 10 + int(c)
            continue
        if op:
            out *= buffer
        else:
            out += buffer
        buffer = 0
        op = c == "*"
    return out


print(sum(map(evaluate1, lines)))


def evaluate2(expr):
    new_expr = ""
    cnt = 0
    buffer = ""
    for c in expr.replace(" ", ""):
        if c == "(":
            if cnt:
                buffer += c
            cnt += 1
        elif c == ")":
            cnt -= 1
            if cnt:
                buffer += c
            else:
                new_expr += str(evaluate2(buffer))
                buffer = ""
        elif cnt:
            buffer += c
        else:
            new_expr += c
    return math.prod(sum(map(int, x.split("+"))) for x in new_expr.split("*"))


print(sum(map(evaluate2, lines)))
