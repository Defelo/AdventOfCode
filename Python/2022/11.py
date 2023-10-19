from utils.parsing import ints
import math


def get_input(puzzle: str) -> tuple[list[tuple[str, int, int, int]], list[list[int]]]:
    monkeys = []
    items = []
    for monkey in puzzle.split("\n\n"):
        lines = monkey.splitlines()
        starting = ints(lines[1])
        (op,) = lines[2].split(maxsplit=3)[3:]
        (test,) = ints(lines[3])
        (tt,) = ints(lines[4])
        (tf,) = ints(lines[5])
        monkeys.append((op, test, tt, tf))
        items.append(starting)
    return monkeys, items


def simulate(monkeys, items, rounds, div3):
    lcm = math.lcm(*(m[1] for m in monkeys))
    cnt = [0] * len(monkeys)
    for _ in range(rounds):
        for i, ((op, test, tt, tf), it) in enumerate(zip(monkeys, items)):
            for j in it:
                cnt[i] += 1
                j = eval(op, {"old": j})
                if div3:
                    j //= 3
                j %= lcm
                items[[tf, tt][j % test == 0]].append(j)
            it.clear()
    cnt.sort()
    return cnt[-1] * cnt[-2]


def part1(puzzle: str):
    monkeys, items = get_input(puzzle)
    return simulate(monkeys, items, 20, True)


def part2(puzzle: str):
    monkeys, items = get_input(puzzle)
    return simulate(monkeys, items, 10000, False)


if __name__ == "__main__":
    from aoc import run

    run(2022, 11, part1, part2)
