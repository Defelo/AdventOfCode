from utils.parsing import ints


def part1(puzzle: str):
    return max(sum(ints(p)) for p in puzzle.split("\n\n"))


def part2(puzzle: str):
    return sum(sorted([sum(ints(p)) for p in puzzle.split("\n\n")], reverse=True)[:3])


if __name__ == "__main__":
    from aoc import run

    run(2022, 1, part1, part2)
