from lib import *

input = read_input(2021, 15)

lines = input.splitlines()


def dijkstra(k):
    grid = [[*map(int, line)] for line in lines]
    w = len(grid[0])
    h = len(grid)
    queue = [(0, 0, 0)]
    visited = set()
    while queue:
        d, x, y = heapq.heappop(queue)

        if (x, y) in visited:
            continue

        visited.add((x, y))

        if (x, y) == (w * k - 1, h * k - 1):
            return d

        for p, q in get_neighbors(x, y, w * k, h * k):
            c = grid[q % h][p % w] + q // h + p // w
            c = (c - 1) % 9 + 1
            heapq.heappush(queue, (d + c, p, q))


print(dijkstra(1))
print(dijkstra(5))
