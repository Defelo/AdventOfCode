from datetime import date

import pyperclip

import aoc
from utils.live import *


def ans(answer):
    print(answer)
    pyperclip.copy(str(answer))


year, day = (t := date.today()).year, t.day

puzzle = aoc.setup(year, day, f="input.txt")
# puzzle = open("example.txt").read()
plines = puzzle.splitlines()

# nums = ints(puzzle)
# nums = [int(x) for x in puzzle.split(",")]

for line in plines:
    match = re.match(r"^$", line)

print(puzzle)
ans()
