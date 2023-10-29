from lib import *

input = read_input(2018, 11)


serial = int(input)
get = lambda x, y: ((((x + 10) * y + serial) * (x + 10)) // 100) % 10 - 5
print(
    ",".join(
        map(
            str,
            max(
                ((x, y) for x in range(1, 299) for y in range(1, 299)),
                key=lambda k: sum(get(k[0] + i, k[1] + j) for i in range(3) for j in range(3)),
            ),
        )
    )
)


serial = int(input)
get = lambda x, y: ((((x + 10) * y + serial) * (x + 10)) // 100) % 10 - 5
ps = [[0] * 301]
for i in range(1, 301):
    lst = [0]
    for j in range(1, 301):
        lst.append(get(j, i) + ps[i - 1][j] + lst[j - 1] - ps[i - 1][j - 1])
    ps.append(lst)

sq = lambda x, y, s: ps[y + s - 1][x + s - 1] - ps[y - 1][x + s - 1] - ps[y + s - 1][x - 1] + ps[y - 1][x - 1]

print(
    ",".join(
        map(
            str,
            max(
                ((i, j, s) for s in range(1, 301) for i in range(1, 302 - s) for j in range(1, 302 - s)),
                key=lambda k: sq(*k),
            ),
        )
    )
)
