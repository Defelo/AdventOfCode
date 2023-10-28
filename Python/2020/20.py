from lib import *

input = read_input(2020, 20)


def edge_to_int(edge):
    x1 = int("".join(edge).replace(".", "0").replace("#", "1"), 2)
    x2 = int("".join(reversed(edge)).replace(".", "0").replace("#", "1"), 2)
    return min(x1, x2)


m = {}
for num, *lines in map(str.splitlines, input.strip().split("\n\n")):
    t = list(zip(*lines))
    num = int(num.split()[1][:-1])
    for e in map(edge_to_int, [lines[0], lines[-1], t[0], t[-1]]):
        m.setdefault(e, []).append(num)

border = [v[0] for k, v in m.items() if len(v) == 1]

print(math.prod(set([x for x in border if border.count(x) == 2])))


op = [1, 0, 3, 2]

monster = """                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """.splitlines()


def rotflip(tile, o):
    c = (3, 1)
    k = True
    while c != o:
        if k:
            tile = list(zip(*tile))
            c = c[::-1]
        else:
            tile = [line[::-1] for line in tile]
            c = (op[c[0]], c[1])
        k = not k

    return list(map("".join, tile))


m = {}
tiles = {}
for num, *lines in map(str.splitlines, input.strip().split("\n\n")):
    t = list(zip(*lines))
    num = int(num.split()[1][:-1])
    tiles[num] = [line[1:-1] for line in lines[1:-1]]
    for i, e in enumerate(map(edge_to_int, [lines[0], lines[-1], t[0], t[-1]])):
        m.setdefault(e, []).append((i, num))

G = {}
for (o1, t1), (o2, t2) in [v for k, v in m.items() if len(v) == 2]:
    G.setdefault(t1, {})[o1] = (o2, t2)
    G.setdefault(t2, {})[o2] = (o1, t1)

corner = next(k for k, v in G.items() if len(v) == 2)
border = {corner}
dim = []
for k in G[corner]:
    c = corner
    x = 1
    while k in G[c]:
        k, c = G[c][k]
        k = op[k]
        border.add(c)
        x += 1
    dim.append(x)

width, height = dim
for k, v in G.items():
    if k in border:
        continue
    for i in range(4):
        v.setdefault(i, None)

tiled_image = [[None] * width for _ in range(height)]
queue = [(corner, 0, 0, *G[corner])]
visited = set()
while queue:
    p, i, j, a, b = queue.pop(0)
    if p in visited:
        continue

    visited.add(p)
    tiled_image[i][j] = rotflip(tiles[p], (a, b))
    for o1, u, v in [(a, i, j + 1), (b, i + 1, j)]:
        if G[p].get(o1) is None:
            continue

        o2, q = G[p][o1]
        if len(G[q]) >= 3:
            G[q].pop(o2)
        if len(G[q]) <= 2:
            if o1 == a:
                x = op[o2]
                (y,) = set(G[q]) - {x, o2}
            else:
                y = op[o2]
                (x,) = set(G[q]) - {y, o2}
            queue.append((q, u, v, x, y))

image = []
for tile_line in tiled_image:
    image += ["".join(line) for line in zip(*tile_line)]

monster_parts = set()
for i in range(4):
    for j in range(4):
        if i in (j, op[j]):
            continue
        m = rotflip(monster, (i, j))
        for y in range(len(image) - len(m) + 1):
            for x in range(len(image[0]) - len(m[0]) + 1):
                p = [(y + i, x + j) for i, line in enumerate(m) for j, c in enumerate(line) if c == "#"]
                if all(image[i][j] == "#" for i, j in p):
                    monster_parts.update(p)

print(sum(line.count("#") for line in image) - len(monster_parts))
