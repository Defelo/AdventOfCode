from itertools import chain
from utils.grid import iter_line
from utils.parsing import ints


def get_input(puzzle: str) -> list[tuple[int, int, int, int]]:
    return [tuple(ints(line)) for line in puzzle.splitlines()]


def part1(puzzle: str):
    inp = get_input(puzzle)
    ty = 2000000
    out = set()
    beacons = set()
    for sx, sy, bx, by in inp:
        d = abs(sx - bx) + abs(sy - by)
        a = sx - (d - abs(sy - ty))
        b = sx + (d - abs(sy - ty))
        out.update(range(a, b + 1))
        if by == ty:
            beacons.add(bx)

    return len(out - beacons)


def part2(puzzle: str):
    inp = get_input(puzzle)
    sensors = []
    minx = 1e1337
    miny = 1e1337
    maxx = -1e1337
    maxy = -1e1337
    for sx, sy, bx, by in inp:
        d = abs(sx - bx) + abs(sy - by)
        sensors.append((sx, sy, d))

        minx = min(sx, minx)
        miny = min(sy, miny)
        maxx = max(sx, maxx)
        maxy = max(sy, maxy)

    for sx, sy, d in sensors:
        for x, y in chain(
            iter_line(sx, sy - d - 1, sx + d + 1, sy),
            iter_line(sx + d + 1, sy, sx, sy + d + 1),
            iter_line(sx, sy + d + 1, sx - d - 1, sy),
            iter_line(sx - d - 1, sy, sx, sy - d - 1),
        ):
            if minx <= x <= maxx and miny <= y <= maxy and all(abs(x - a) + abs(y - b) > d for a, b, d in sensors):
                return x * 4000000 + y


if __name__ == "__main__":
    from aoc import run

    run(2022, 15, part1, part2)
