def most_common(lst, values=None):
    return max(values or set(lst), key=lst.count)


def least_common(lst, values=None):
    return min(values or set(lst), key=lst.count)


def minmax(lst, key=None):
    return min(lst, key=key), max(lst, key=key)


def irange(start=0, step=1):
    i = start
    while True:
        yield i
        i += step


def sliding_window(seq, n=2):
    lst = []
    for elem in seq:
        lst.append(elem)
        if len(lst) == n:
            yield tuple(lst)
            lst.pop(0)
