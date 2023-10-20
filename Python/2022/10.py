from lib import *

input = read_input(2022, 10)

instructions = [next(iter(ints(line)), None) for line in input.splitlines()]


def execute():
    x = 1
    i = 0
    for inst in instructions:
        yield i, x
        i += 1
        if inst is not None:
            yield i, x
            i += 1
            x += inst


print(sum((i + 1) * x for i, x in execute() if i % 40 == 19))
print(parse_ascii({(i // 40, i % 40) for i, x in execute() if abs(i % 40 - x) <= 1}))
