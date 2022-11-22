import ast
import collections
import functools
import graphlib
import heapq
import io
import itertools
import json
import math
import operator
import re
import statistics

import numpy as np

heappush = heapq.heappush
heappop = heapq.heappop
reduce = functools.reduce
Counter = collections.Counter

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
        if ((x := ox + dx) or 1) and (w is None or x in range(w))
        and ((y := oy + dy) or 1) and (h is None or y in range(h))
    ]


def minmax(lst, key=None):
    return min(lst, key=key), max(lst, key=key)


def product(it):
    return functools.reduce(operator.mul, it)


def rotate_left(x, y):
    return y, -x


def rotate_right(x, y):
    return -y, x


def rotate_matrix_left(matrix):
    return [*zip(*matrix)][::-1]


def rotate_matrix_right(matrix):
    return [*zip(*matrix[::-1])]


def irange(start=0, step=1):
    i = start
    while True:
        yield i
        i += step


class UnionFind:
    def __init__(self, n):
        self.parents = [*range(n)]

    def find(self, x):
        if self.parents[x] == x:
            return x
        self.parents[x] = self.find(self.parents[x])
        return self.parents[x]

    def merge(self, x, y):
        self.parents[self.find(x)] = self.find(y)

    def connected(self, x, y):
        return self.find(x) == self.find(y)

    def count(self):
        return len(set(map(self.find, range(len(self.parents)))))

    def components(self):
        out = {}
        for parent, i in zip(map(self.find, range(len(self.parents))), range(len(self.parents))):
            out.setdefault(parent, []).append(i)

        return out


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


def sliding_window(seq, n=2):
    lst = []
    for elem in seq:
        lst.append(elem)
        if len(lst) == n:
            yield tuple(lst)
            lst.pop(0)


def transpose(matrix):
    return [*zip(*matrix)]


def most_common(lst, values=None):
    return max(values or set(lst), key=lst.count)


def least_common(lst, values=None):
    return min(values or set(lst), key=lst.count)


def extended_gcd(a, b):
    if a == 0:
        return b, 0, 1

    gcd, x1, y1 = extended_gcd(b % a, a)

    x = y1 - (b // a) * x1
    y = x1

    return gcd, x, y


def chinese_remainder(n, a):
    s = 0
    prod = functools.reduce(int.__mul__, n)
    for n_i, a_i in zip(n, a):
        p = prod // n_i
        s += a_i * extended_gcd(p, n_i)[1] * p
    return s % prod


# __all__ = ["itertools", "collections", "functools", "re", "graphlib", "operator", "heapq", "io", "json", "ast", "np"]
# __all__ += dir()
