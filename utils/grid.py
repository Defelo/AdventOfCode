NEIGH_DICT = {
    "N": (0, -1),
    "E": (1, 0),
    "S": (0, 1),
    "W": (-1, 0),
    "U": (0, -1),
    "D": (0, 1),
    "L": (-1, 0),
    "R": (1, 0),
}

NEIGH_DIRECT = [*{*NEIGH_DICT.values()}]
NEIGH_DIAG = [tuple(NEIGH_DICT[a][i] + NEIGH_DICT[b][i] for i in range(2)) for a, b in zip("NESW", "ESWN")]


def get_neighbors(ox=0, oy=0, w=None, h=None, diag=False, include_self=False):
    return [
        (x, y)
        for dx, dy in NEIGH_DIRECT + (NEIGH_DIAG * diag) + ([(0, 0)] * include_self)
        if ((x := ox + dx) or 1)
        and (w is None or x in range(w))
        and ((y := oy + dy) or 1)
        and (h is None or y in range(h))
    ]


def rotate_left(x, y):
    return y, -x


def rotate_right(x, y):
    return -y, x


def rotate_matrix_left(matrix):
    return [*zip(*matrix)][::-1]


def rotate_matrix_right(matrix):
    return [*zip(*matrix[::-1])]


def iter_line(x1, y1, x2, y2):
    xr = range(min(x1, x2), max(x1, x2) + 1)
    if x1 > x2:
        xr = xr[::-1]

    yr = range(min(y1, y2), max(y1, y2) + 1)
    if y1 > y2:
        yr = yr[::-1]

    if x1 == x2:
        xr = [x1] * len(yr)
    if y1 == y2:
        yr = [y1] * len(xr)

    for x, y in zip(xr, yr):
        yield x, y


def transpose(matrix):
    return [*zip(*matrix)]
