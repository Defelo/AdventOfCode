from utils.grid import get_neighbors


def get_input(puzzle: str) -> tuple[tuple[int, int], tuple[int, int], list[list[int]]]:
    grid = []
    start = None
    end = None
    for i, line in enumerate(puzzle.splitlines()):
        grid.append([ord(c) - 97 if c not in "SE" else {"S": 0, "E": 25}[c] for c in line])
        if "S" in line:
            start = line.index("S"), i
        if "E" in line:
            end = line.index("E"), i
    assert start and end
    return start, end, grid


def bfs(start, grid, target, step):
    queue: list[tuple[int, int, int]] = [(0, *start)]
    visited = set()
    while queue:
        d, x, y = queue.pop(0)

        if target(x, y):
            return d

        if (x, y) in visited:
            continue
        visited.add((x, y))

        for p, q in get_neighbors(x, y, len(grid[0]), len(grid)):
            if step(x, y, p, q) and (p, q) not in visited:
                queue.append((d + 1, p, q))


def part1(puzzle: str):
    start, end, grid = get_input(puzzle)
    return bfs(start, grid, lambda x, y: (x, y) == end, lambda x, y, p, q: grid[q][p] - grid[y][x] <= 1)


def part2(puzzle: str):
    _, end, grid = get_input(puzzle)
    return bfs(end, grid, lambda x, y: grid[y][x] == 0, lambda x, y, p, q: grid[y][x] - grid[q][p] <= 1)


if __name__ == "__main__":
    from aoc import run

    run(2022, 12, part1, part2)
