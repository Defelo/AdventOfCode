from lib import *

input = read_input(2022, 9)

lines = [(*NEIGH_DICT[line[0]], int(line[2:])) for line in input.splitlines()]


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


print(solve(lines, 2))
print(solve(lines, 10))
