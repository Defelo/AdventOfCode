def get_input(puzzle: str) -> str:
    return puzzle


def part1(puzzle: str):
    for i in range(4, len(puzzle)):
        if len(set(puzzle[i - 4 : i])) == 4:
            return i


def part2(puzzle: str):
    for i in range(14, len(puzzle)):
        if len(set(puzzle[i - 14 : i])) == 14:
            return i


if __name__ == "__main__":
    from aoc import run

    run(2022, 6, part1, part2)
