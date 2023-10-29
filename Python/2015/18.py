from lib import *

input = read_input(2015, 18)

gol = input.splitlines()


for _ in range(100):
    gol = [
        [
            ".#"[
                (
                    cnt := sum(
                        gol[p][q] == "#"
                        for r in range(3)
                        for s in range(3)
                        if (r, s) != (1, 1) and 0 <= (p := i - 1 + r) < len(gol) and 0 <= (q := j - 1 + s) < len(row)
                    )
                )
                in (2, 3)
                and x == "#"
                or x == "."
                and cnt == 3
            ]
            for j, x in enumerate(row)
        ]
        for i, row in enumerate(gol)
    ]

print(sum(x == "#" for row in gol for x in row))


gol = list(map(list, input.splitlines()))
gol[0][0] = gol[0][-1] = gol[-1][0] = gol[-1][-1] = "#"
for _ in range(100):
    gol = [
        [
            ".#"[
                (
                    cnt := sum(
                        gol[p][q] == "#"
                        for r in range(3)
                        for s in range(3)
                        if (r, s) != (1, 1) and 0 <= (p := i - 1 + r) < len(gol) and 0 <= (q := j - 1 + s) < len(row)
                    )
                )
                in (2, 3)
                and x == "#"
                or x == "."
                and cnt == 3
            ]
            for j, x in enumerate(row)
        ]
        for i, row in enumerate(gol)
    ]
    gol[0][0] = gol[0][-1] = gol[-1][0] = gol[-1][-1] = "#"


print(sum(x == "#" for row in gol for x in row))
