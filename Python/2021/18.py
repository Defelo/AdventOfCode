from lib import *

input = read_input(2021, 18)

lines = input.splitlines()

parent = lambda n: n >> 1
left = lambda n: n << 1
right = lambda n: n << 1 | 1


def parse(lst):
    out = [None] * 64

    def fill(l, n):
        if not isinstance(l, list):
            out[n] = l
        else:
            fill(l[0], left(n))
            fill(l[1], right(n))

    fill(lst, 1)
    return out


def explode(lst):
    def move_up(i, x):
        while lst[i] is None:
            i = parent(i)

        lst[i] += x

    for n in range(1 << 4, 1 << 5):
        if lst[n] is not None:
            continue

        l = lst[a := left(n)]
        r = lst[b := right(n)]
        if l is None and r is None:
            continue

        lst[a] = None
        lst[b] = None
        lst[n] = 0

        if a - 1 >= 1 << 5:
            move_up(a - 1, l)
        if b + 1 < 1 << 6:
            move_up(b + 1, r)

        return True

    return False


def split(lst, n=1):
    if (x := lst[n]) is None:
        return split(lst, left(n)) or split(lst, right(n))

    if x < 10:
        return False

    k = x >> 1
    lst[n] = None
    lst[left(n)] = k
    lst[right(n)] = x - k
    return True


def add(a, b):
    out = [None, None]
    for i in range(6):
        out += a[1 << i : 1 << i + 1]
        out += b[1 << i : 1 << i + 1]

    while explode(out) or split(out):
        pass

    return out


def magnitude(lst, n=1):
    if lst[n] is not None:
        return lst[n]

    return 3 * magnitude(lst, left(n)) + 2 * magnitude(lst, right(n))


def mk_list(lst, n=1):
    if lst[n] is not None:
        return lst[n]

    return [mk_list(lst, left(n)), mk_list(lst, right(n))]


print(magnitude(reduce(add, (parse(ast.literal_eval(line)) for line in lines))))


out = 0

for a in lines:
    for b in lines:
        if a == b:
            continue

        out = max(out, magnitude(add(parse(ast.literal_eval(a)), parse(ast.literal_eval(b)))))

print(out)
