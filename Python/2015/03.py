from lib import *

input = read_input(2015, 3)


def get_houses(instructions):
    houses = [(x := 0, y := 0)]
    for c in instructions:
        x += c == ">"
        x -= c == "<"
        y += c == "v"
        y -= c == "^"
        houses.append((x, y))
    return houses


print(len(set(get_houses(input))))
print(len(set(get_houses(input[::2])) | set(get_houses(input[1::2]))))
