from lib import *

input = read_input(2015, 6)


def parse_instructions(instructions):
    out = []

    for line in instructions:
        inst, *coords = re.match(r"^([a-z ]+) (\d+),(\d+) through (\d+),(\d+)$", line).groups()

        out.append((["turn off", "turn on", "toggle"].index(inst), *map(int, coords)))

    return out


lights = [[0 for _ in range(1000)] for _ in range(1000)]
for inst, x1, y1, x2, y2 in parse_instructions(input.splitlines()):
    for y in range(y1, y2 + 1):
        for x in range(x1, x2 + 1):
            lights[y][x] = 1 - lights[y][x] if inst == 2 else inst
print(sum(map(sum, lights)))


lights = [[0 for _ in range(1000)] for _ in range(1000)]
for inst, x1, y1, x2, y2 in parse_instructions(input.splitlines()):
    for y in range(y1, y2 + 1):
        for x in range(x1, x2 + 1):
            lights[y][x] = max(0, lights[y][x] + (inst or -1))
print(sum(map(sum, lights)))
