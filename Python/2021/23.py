from lib import *

input = read_input(2021, 23)


def generate_moves(rooms, hallway, n):
    def check_hallway(start, end):
        return all(hallway[i] is None or i == start for i in range(min(start, end), max(start, end) + 1))

    def push_room(idx, elem):
        return rooms[:idx] + (rooms[idx] + (elem,),) + rooms[idx + 1 :]

    def pop_room(idx):
        return rooms[:idx] + (rooms[idx][:-1],) + rooms[idx + 1 :]

    def set_hallway(idx, elem):
        return hallway[:idx] + (elem,) + hallway[idx + 1 :]

    for i, c in enumerate(hallway):
        if c is None:
            continue

        dst = "ABCD".index(c)

        if any(x != c for x in rooms[dst]):
            continue

        if not check_hallway(i, 2 + 2 * dst):
            continue

        dist = abs(2 + 2 * dst - i) + (n - len(rooms[dst]))

        yield dist * 10**dst, push_room(dst, c), set_hallway(i, None)

        return

    for i in range(4):
        if all(x == "ABCD"[i] for x in rooms[i]):
            continue

        c = rooms[i][-1]

        src = 2 + 2 * i

        dst = "ABCD".index(c)

        for j in [0, 1, 3, 5, 7, 9, 10]:
            if not check_hallway(src, j):
                continue

            dist = (1 + n - len(rooms[i])) + abs(src - j)

            yield dist * 10**dst, pop_room(i), set_hallway(j, c)


def solve(part2):
    lines = input.splitlines()[3:1:-1]

    if part2:
        lines.insert(1, "  #D#B#A#C#")

        lines.insert(2, "  #D#C#B#A#")

    n = len(lines)

    queue = [(0, cnt := 0, tuple([*zip(*lines)][3:-1:2]), (None,) * 11)]

    visited = set()

    while queue:
        energy, _, rooms, hallway = heappop(queue)

        if (rooms, hallway) in visited:
            continue

        visited.add((rooms, hallway))

        if rooms == tuple((c,) * n for c in "ABCD"):
            return energy

        for c, r, h in generate_moves(rooms, hallway, n):
            cnt += 1

            heappush(queue, (energy + c, cnt, r, h))


print(solve(False))
print(solve(True))
