from lib import *

input = read_input(2022, 13)

pairs = [tuple(map(eval, pair.splitlines())) for pair in input.split("\n\n")]


def compare(a, b):
    if isinstance(a, int) and isinstance(b, int):
        if a < b:
            return True
        elif a > b:
            return False
        else:
            return None
    if isinstance(a, int):
        a = [a]
    if isinstance(b, int):
        b = [b]
    for x, y in zip(a, b):
        if (z := compare(x, y)) is not None:
            return z
    if len(a) < len(b):
        return True
    elif len(a) > len(b):
        return False
    else:
        return None


print(sum((i + 1) * (compare(a, b) is True) for i, (a, b) in enumerate(pairs)))


packets = [[[2]], [[6]]] + [x for a, b in pairs for x in [a, b]]
out = sum(compare(packet, packets[0]) is True for packet in packets) + 1
out *= sum(compare(packet, packets[1]) is True for packet in packets) + 1
print(out)
