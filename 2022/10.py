from utils.parsing import ints


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
    lines = []
    for i, x in execute(get_input(puzzle)):
        if i % 40 == 0:
            lines.append("")
        lines[-1] += " #"[abs(i % 40 - x) <= 1] * 2
    return "\n" + "\n".join(lines)


if __name__ == "__main__":
    from aoc import run

    run(2022, 10, part1, part2)
