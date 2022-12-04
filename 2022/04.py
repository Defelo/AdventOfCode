from utils.parsing import pints


def part1(puzzle: str):
    return sum(a <= c <= d <= b or c <= a <= b <= d for a, b, c, d in map(pints, puzzle.splitlines()))


def part2(puzzle: str):
    return sum(d >= a and c <= b for a, b, c, d in map(pints, puzzle.splitlines()))


if __name__ == "__main__":
    from aoc import run

    run(2022, 4, part1, part2)
