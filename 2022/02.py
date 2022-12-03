def part1(puzzle: str):
    out = 0
    for line in puzzle.splitlines():
        a, b = line.split()
        a = ord(a) - ord("A")
        b = ord(b) - ord("X")
        w = (a - b) % 3
        if w == 0:
            out += 3 + b + 1
        elif w == 1:
            out += b + 1
        elif w == 2:
            out += 6 + b + 1
    return out


def part2(puzzle: str):
    out = 0
    for line in puzzle.splitlines():
        a, b = line.split()
        a = ord(a) - ord("A")
        b = ord(b) - ord("X")
        w = (a + b - 1) % 3
        if b == 0:
            out += w + 1
        elif b == 1:
            out += 3 + w + 1
        elif b == 2:
            out += 6 + w + 1
    return out


if __name__ == "__main__":
    from aoc import run

    run(2022, 2, part1, part2)
