from utils.parsing import ints, parse_ascii


def get_input(puzzle: str) -> list[int | None]:
    return [next(iter(ints(line)), None) for line in puzzle.splitlines()]


def execute(instructions: list[int | None]):
    x = 1
    i = 0
    for inst in instructions:
        yield i, x
        i += 1
        if inst is not None:
            yield i, x
            i += 1
            x += inst


def part1(puzzle: str):
    return sum((i + 1) * x for i, x in execute(get_input(puzzle)) if i % 40 == 19)


def part2(puzzle: str):
    return parse_ascii({(i // 40, i % 40) for i, x in execute(get_input(puzzle)) if abs(i % 40 - x) <= 1})


if __name__ == "__main__":
    from aoc import run

    run(2022, 10, part1, part2)
