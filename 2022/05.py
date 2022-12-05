from utils.parsing import pints


def get_input(puzzle: str) -> tuple[list[list[str]], list[tuple[int, int, int]]]:
    initial, instructions = puzzle.split("\n\n")
    nums, *initial = initial.splitlines()[::-1]

    stacks: list[list[str]] = [[] for _ in pints(nums)]
    for line in initial:
        for i in range(0, len(line), 4):
            if line[i + 1] != " ":
                stacks[i // 4].append(line[i + 1])

    return stacks, [(cnt, i - 1, j - 1) for cnt, i, j in map(pints, instructions.splitlines())]


def part1(puzzle: str):
    stacks, instructions = get_input(puzzle)
    for cnt, i, j in instructions:
        for _ in range(cnt):
            stacks[j].append(stacks[i].pop())
    return "".join(s[-1] for s in stacks)


def part2(puzzle: str):
    stacks, instructions = get_input(puzzle)
    for cnt, i, j in instructions:
        stacks[j] += stacks[i][-cnt:]
        del stacks[i][-cnt:]
    return "".join(s[-1] for s in stacks)


if __name__ == "__main__":
    from aoc import run

    run(2022, 5, part1, part2)
