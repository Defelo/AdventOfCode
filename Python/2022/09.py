from utils.grid import NEIGH_DICT


def get_input(puzzle: str) -> list[tuple[int, int, int]]:
    return [(*NEIGH_DICT[line[0]], int(line[2:])) for line in puzzle.splitlines()]


def solve(lines, n):
    knots: list[tuple[int, int]] = [(0, 0) for _ in range(n)]
    visited = {knots[-1]}
    for dx, dy, cnt in lines:
        for _ in range(cnt):
            knots[0] = knots[0][0] + dx, knots[0][1] + dy
            for i in range(1, n):
                p = knots[i - 1][0] - knots[i][0]
                q = knots[i - 1][1] - knots[i][1]
                if abs(p) <= 1 and abs(q) <= 1:
                    continue
                knots[i] = knots[i][0] + max(-1, min(p, 1)), knots[i][1] + max(-1, min(q, 1))
            visited.add(knots[-1])
    return len(visited)


def part1(puzzle: str):
    lines = get_input(puzzle)
    return solve(lines, 2)


def part2(puzzle: str):
    lines = get_input(puzzle)
    return solve(lines, 10)


if __name__ == "__main__":
    from aoc import run

    run(2022, 9, part1, part2)
