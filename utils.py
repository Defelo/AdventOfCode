import itertools
import collections


__all__ = ["itertools", "collections", "iter_line"]


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
