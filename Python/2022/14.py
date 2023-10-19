from utils.grid import iter_line
from utils.list import sliding_window


def get_input(puzzle: str) -> tuple[set[tuple[int, int]], int]:
    rock = set()
    maxy = 0
    for line in puzzle.splitlines():
        for a, b in sliding_window(line.split(" -> ")):
            for x, y in iter_line(*map(int, a.split(",")), *map(int, b.split(","))):
                rock.add((x, y))
                maxy = max(y, maxy)
    return rock, maxy


def simulate(x, y, air, maxy):
    while y <= maxy:
        if air(x, y + 1):
            y += 1
        elif air(x - 1, y + 1):
            x -= 1
            y += 1
        elif air(x + 1, y + 1):
            x += 1
            y += 1
        else:
            return x, y
    return None


def part1(puzzle: str):
    rock, maxy = get_input(puzzle)
    sand = set()
    while s := simulate(500, 0, lambda x, y: (x, y) not in rock and (x, y) not in sand, maxy):
        sand.add(s)
    return len(sand)


def part2(puzzle: str):
    rock, maxy = get_input(puzzle)
    sand = set()
    while (500, 0) not in sand:
        sand.add(simulate(500, 0, lambda x, y: y < maxy + 2 and (x, y) not in rock and (x, y) not in sand, maxy + 2))
    return len(sand)


if __name__ == "__main__":
    from aoc import run

    run(2022, 14, part1, part2)
