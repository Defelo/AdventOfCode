from functools import cache, reduce
import re


def get_input(puzzle: str) -> tuple[list[int], list[set[int]]]:
    n = len(puzzle.splitlines())
    graph = [set() for _ in range(n)]
    rates = [0] * n
    names = {"AA": 0}

    def name(n: str) -> int:
        return names.setdefault(n, len(names))

    for line in puzzle.splitlines():
        v, r, n = re.match(r"^Valve (.+) has flow rate=(\d+); tunnels? leads? to valves? (.+)$", line).groups()  # type: ignore
        v = name(v)
        r = int(r)
        n = list(map(name, n.split(", ")))
        rates[v] = r
        graph[v].update(n)
    return rates, graph


def part1(puzzle: str):
    rates, graph = get_input(puzzle)
    n = len(rates)
    dist = {i: {j: 0 if i == j else 1 if j in graph[i] else 1e1337 for j in range(n)} for i in range(n)}
    for k in range(n):
        for i in range(n):
            for j in range(n):
                dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])

    @cache
    def solve(p, time, closed):
        if time <= 0:
            return 0

        out = 0
        for q in range(n):
            if closed & 1 << q == 0:
                continue
            t = time - dist[p][q] - 1
            out = max(out, solve(q, t, closed & ~(1 << q)) + rates[q] * t)
        return out

    return solve(0, 30, reduce(lambda acc, x: acc | 1 << x, (i for i in range(n) if rates[i]), 0))


def part2(puzzle: str):
    rates, graph = get_input(puzzle)
    n = len(rates)
    dist = {i: {j: 0 if i == j else 1 if j in graph[i] else 1e1337 for j in range(n)} for i in range(n)}
    for k in range(n):
        for i in range(n):
            for j in range(n):
                dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])

    @cache
    def solve(p, time, closed):
        if time <= 0:
            return 0

        out = 0
        for q in range(n):
            if closed & 1 << q == 0:
                continue
            t = time - dist[p][q] - 1
            out = max(out, solve(q, t, closed & ~(1 << q)) + rates[q] * t)
        return out

    out = 0
    valves = [i for i in range(n) if rates[i]]
    for s in range(1 << len(valves)):
        a = solve(0, 26, reduce(lambda acc, x: acc | 1 << x, (j for i, j in enumerate(valves) if s & 1 << i), 0))
        b = solve(0, 26, reduce(lambda acc, x: acc | 1 << x, (j for i, j in enumerate(valves) if s & 1 << i == 0), 0))
        out = max(out, a + b)
    return out


if __name__ == "__main__":
    from aoc import run

    run(2022, 16, part1, part2)
