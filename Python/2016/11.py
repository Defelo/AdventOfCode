from lib import *

input = read_input(2016, 11)


names = {}
get_id = lambda name: names.setdefault(name, len(names))
generators = {}
microchips = {}
for i, line in enumerate(input.splitlines()):
    for g in map(get_id, re.findall(r"([a-z]+) generator", line)):
        generators[g] = i

    for m in map(get_id, re.findall(r"([a-z]+)-compatible microchip", line)):
        microchips[m] = i

generators = tuple([generators[i] for i in range(len(generators))])
microchips = tuple([microchips[i] for i in range(len(microchips))])

sub = lambda lst, i, x: lst[:i] + (x,) + lst[i + 1 :]


def is_valid(generators, microchips):
    for i, e in enumerate(microchips):
        if not 0 <= e < 4:
            return False
        if generators[i] == e:
            continue
        if e in generators:
            return False
    return True


def solve():
    def add_to_queue(d, e, g, m):
        g, m = map(tuple, zip(*sorted(zip(g, m))))
        if is_valid(g, m):
            queue.append((d, e, g, m))

    queue = [(0, 0, generators, microchips)]
    visited = set()
    while queue:
        dist, el, ge, mi = queue.pop(0)

        if (el, ge, mi) in visited:
            continue

        visited.add((el, ge, mi))

        if ge == mi == (3,) * len(ge):
            return dist

        for d in [-1, 1]:
            if not 0 <= (e := el + d) < 4:
                continue

            if d == -1 and all(x >= el for x in ge + mi):
                continue

            for i in range(len(ge)):
                if ge[i] == el:
                    add_to_queue(dist + 1, e, sub(ge, i, e), mi)

                if mi[i] == el:
                    add_to_queue(dist + 1, e, ge, sub(mi, i, e))

                if ge[i] == mi[i] == el:
                    add_to_queue(dist + 1, e, sub(ge, i, e), sub(mi, i, e))

                for j in range(i + 1, len(ge)):
                    if ge[i] == ge[j] == el:
                        add_to_queue(dist + 1, e, sub(sub(ge, j, e), i, e), mi)

                    if mi[i] == mi[j] == el:
                        add_to_queue(dist + 1, e, ge, sub(sub(mi, j, e), i, e))


print(solve())


names = {}
generators = {}
microchips = {}
for i, line in enumerate(input.splitlines()):
    for g in map(get_id, re.findall(r"([a-z]+) generator", line)):
        generators[g] = i

    for m in map(get_id, re.findall(r"([a-z]+)-compatible microchip", line)):
        microchips[m] = i

for i in map(get_id, "ab"):
    generators[i] = microchips[i] = 0

generators = tuple([generators[i] for i in range(len(generators))])
microchips = tuple([microchips[i] for i in range(len(microchips))])
print(solve())
