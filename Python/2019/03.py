from lib import *

input = read_input(2019, 3)


def parse_wire(wire):
    x = 0
    y = 0
    out = set()
    for e in wire.split(","):
        dx, dy = {"U": (0, -1), "D": (0, 1), "L": (-1, 0), "R": (1, 0)}[e[0]]
        for _ in range(int(e[1:])):
            x += dx
            y += dy
            out.add((x, y))
    return out


wire1, wire2 = map(parse_wire, input.splitlines())
print(min(abs(x) + abs(y) for x, y in (wire1 & wire2)))


def parse_wire(wire):
    x = 0
    y = 0
    out = {}
    count = 0
    for e in wire.split(","):
        dx, dy = {"U": (0, -1), "D": (0, 1), "L": (-1, 0), "R": (1, 0)}[e[0]]
        for _ in range(int(e[1:])):
            x += dx
            y += dy
            count += 1
            if (x, y) not in out:
                out[(x, y)] = count
    return out


wire1, wire2 = map(parse_wire, input.splitlines())
print(min(wire1[intersection] + wire2[intersection] for intersection in (wire1.keys() & wire2.keys())))
