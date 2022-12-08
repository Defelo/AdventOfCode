from utils.grid import NEIGH_DIRECT


def get_input(puzzle: str) -> list[list[int]]:
    return [[*map(int, line)] for line in puzzle.splitlines()]


def part1(puzzle: str):
    grid = get_input(puzzle)
    w, h = len(grid[0]), len(grid)
    visible = set()
    for dx, dy in NEIGH_DIRECT:
        for y in range(h):
            for x in range(w):
                i = y + dy
                j = x + dx
                while i in range(h) and j in range(w):
                    if grid[i][j] >= grid[y][x]:
                        break
                    i += dy
                    j += dx
                else:
                    visible.add((x, y))
    return len(visible)


def part2(puzzle: str):
    grid = get_input(puzzle)
    out = 0
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            d = 1
            for dx, dy in NEIGH_DIRECT:
                i = y + dy
                j = x + dx
                s = 0
                while i in range(len(grid)) and j in range(len(grid[0])):
                    s += 1
                    if grid[i][j] >= grid[y][x]:
                        break
                    i += dy
                    j += dx
                d *= s
            out = max(out, d)
    return out


if __name__ == "__main__":
    from aoc import run

    run(2022, 8, part1, part2)
