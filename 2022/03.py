def part1(puzzle: str):
    return sum(
        ord((x := ({*line[: len(line) // 2]} & {*line[len(line) // 2 :]}).pop()).upper()) - 64 + 26 * x.isupper()
        for line in puzzle.splitlines()
    )


def part2(puzzle: str):
    lines = puzzle.splitlines()
    return sum(
        ord((x := ({*lines[i]} & {*lines[i + 1]} & {*lines[i + 2]}).pop()).upper()) - 64 + 26 * x.isupper()
        for i in range(0, len(lines), 3)
    )


if __name__ == "__main__":
    from aoc import run

    run(2022, 3, part1, part2)
