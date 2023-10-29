from lib import *

input = read_input(2018, 22)

lines = input.splitlines()

depth = int(lines[0].split()[1])

tx, ty = map(int, lines[1].split()[1].split(","))


dp = {}


def get_geologic_index(x, y):
    if (x, y) == (tx, ty):
        return 0
    if y == 0:
        return x * 16807
    if x == 0:
        return y * 48271
    if (x, y) not in dp:
        dp[(x, y)] = get_erosion_level(x - 1, y) * get_erosion_level(x, y - 1)
    return dp[(x, y)]


def get_erosion_level(x, y):
    return (get_geologic_index(x, y) + depth) % 20183


print(sum(get_erosion_level(x, y) % 3 for y in range(ty + 1) for x in range(tx + 1)))


queue = [(0, 0, 0, 1)]
visited = set()
while queue:
    d, x, y, e = heapq.heappop(queue)
    if (x, y, e) in visited:
        continue

    visited.add((x, y, e))
    if (x, y, e) == (tx, ty, 1):
        print(d)
        break

    t = get_erosion_level(x, y) % 3
    heapq.heappush(queue, (d + 7, x, y, (-e - t) % 3))
    for p, q in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
        if p < 0 or q < 0:
            continue

        t = get_erosion_level(p, q) % 3
        if t == e:
            continue

        heapq.heappush(queue, (d + 1, p, q, e))
