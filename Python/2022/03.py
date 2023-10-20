from lib import *

input = read_input(2022, 3)

lines = input.splitlines()

print(
    sum(
        ord((x := ({*line[: len(line) // 2]} & {*line[len(line) // 2 :]}).pop()).upper()) - 64 + 26 * x.isupper()
        for line in lines
    )
)

print(
    sum(
        ord((x := ({*lines[i]} & {*lines[i + 1]} & {*lines[i + 2]}).pop()).upper()) - 64 + 26 * x.isupper()
        for i in range(0, len(lines), 3)
    )
)
