def get_input(puzzle: str) -> list[tuple[list, list]]:
    out = []
    for pair in puzzle.split("\n\n"):
        out.append(tuple(map(eval, pair.splitlines())))
    return out


def compare(a, b):
    if isinstance(a, int) and isinstance(b, int):
        if a < b:
            return True
        elif a > b:
            return False
        else:
            return None
    if isinstance(a, int):
        a = [a]
    if isinstance(b, int):
        b = [b]
    for x, y in zip(a, b):
        if (z := compare(x, y)) is not None:
            return z
    if len(a) < len(b):
        return True
    elif len(a) > len(b):
        return False
    else:
        return None


def part1(puzzle: str):
    return sum((i + 1) * (compare(a, b) is True) for i, (a, b) in enumerate(get_input(puzzle)))


def part2(puzzle: str):
    packets = [[[2]], [[6]]] + [x for a, b in get_input(puzzle) for x in [a, b]]

    out = sum(compare(packet, packets[0]) is True for packet in packets) + 1
    out *= sum(compare(packet, packets[1]) is True for packet in packets) + 1
    return out


if __name__ == "__main__":
    from aoc import run

    run(2022, 13, part1, part2)
