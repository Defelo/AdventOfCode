SHAPES = [
    [[1, 1, 1, 1]],
    [[0, 1, 0], [1, 1, 1], [0, 1, 0]],
    [[0, 0, 1], [0, 0, 1], [1, 1, 1]],
    [[1], [1], [1], [1]],
    [[1, 1], [1, 1]],
]

WIDTH = 7
START_X = 2
START_Y = 3


def get_input(puzzle: str) -> str:
    return puzzle


def part1(puzzle: str):
    grid = set()
    height = 0
    sh = 0
    st = 0

    def test(s, x, y):
        return (
            y - len(s) >= -1
            and x >= 0
            and x + len(s[0]) <= WIDTH
            and all((y - i, x + j) not in grid for i, r in enumerate(s) for j, k in enumerate(r) if k)
        )

    def add(s, x, y):
        grid.update((y - i, x + j) for i, r in enumerate(s) for j, k in enumerate(r) if k)

    for _ in range(2022):
        s = SHAPES[sh]
        sh = (sh + 1) % len(SHAPES)
        x = START_X
        y = height + START_Y + len(s) - 1
        while True:
            d = {"<": -1, ">": 1}[puzzle[st]]
            st = (st + 1) % len(puzzle)
            if test(s, x + d, y):
                x += d
            if test(s, x, y - 1):
                y -= 1
            else:
                add(s, x, y)
                height = max(y + 1, height)
                break
    return height


def part2(puzzle: str):
    grid = set()
    height = 0
    st = 0
    sh = 0

    def test(s, x, y):
        return (
            y - len(s) >= -1
            and x >= 0
            and x + len(s[0]) <= WIDTH
            and all((y - i, x + j) not in grid for i, r in enumerate(s) for j, k in enumerate(r) if k)
        )

    def add(s, x, y):
        grid.update((y - i, x + j) for i, r in enumerate(s) for j, k in enumerate(r) if k)

    seen = []
    idx = {}
    heights = []

    round = 0
    while True:
        k = sh, st
        seen.append(k)
        heights.append(height)
        if k in idx:
            l = len(seen) - idx[k] - 1
            if seen[-l:] == seen[-2 * l : -l]:
                break
        idx[k] = round
        s = SHAPES[sh]
        sh = (sh + 1) % len(SHAPES)
        x = START_X
        y = height + START_Y + len(s) - 1
        while True:
            d = {"<": -1, ">": 1}[puzzle[st]]
            st = (st + 1) % len(puzzle)
            if test(s, x + d, y):
                x += d
            if test(s, x, y - 1):
                y -= 1
            else:
                add(s, x, y)
                height = max(y + 1, height)
                break
        round += 1

    left = 1000000000000 - round
    height += left // l * (heights[-1] - heights[-l - 1])
    height += heights[-l - 1 + left % l] - heights[-l - 1]
    return height


if __name__ == "__main__":
    from aoc import run

    run(2022, 17, part1, part2)
