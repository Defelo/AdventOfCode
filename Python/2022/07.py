import re


def get_input(puzzle: str) -> list[str]:
    return puzzle.splitlines()


def get_dir_sizes(lines: list[str]) -> dict[tuple[str, ...], int]:
    out = {}
    seen = set()
    pwd = ()
    for line in lines:
        if match := re.match(r"^\$ cd (.+)$", line):
            d = match[1]
            if d == "/":
                pwd = ()
            elif d == "..":
                pwd = pwd[:-1]
            else:
                pwd = (*pwd, d)
        elif match := re.match(r"^(\d+) (.+)$", line):
            s = int(match[1])
            n = match[2]

            if (pwd, n) in seen:
                continue
            seen.add((pwd, n))

            d = pwd
            while True:
                out[d] = out.get(d, 0) + s
                if not d:
                    break
                d = d[:-1]

    return out


def part1(puzzle: str):
    sizes = get_dir_sizes(get_input(puzzle))
    return sum(s for s in sizes.values() if s <= 100000)


def part2(puzzle: str):
    sizes = get_dir_sizes(get_input(puzzle))
    free = 70000000 - sizes[()]
    return min(s for s in sizes.values() if s >= 30000000 - free)


if __name__ == "__main__":
    from aoc import run

    run(2022, 7, part1, part2)
