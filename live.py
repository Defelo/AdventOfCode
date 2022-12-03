from datetime import date
import sys

import pyperclip

import aoc
from utils.live import *


def ans(answer):
    print(answer)
    pyperclip.copy(str(answer))


year, day = (t := date.today()).year, t.day

puzzle = aoc.setup(year, day, f=sys.argv[1] if len(sys.argv) > 1 else "input.txt")
plines = puzzle.splitlines()

# nums = ints(puzzle)
# nums = [int(x) for x in puzzle.split(",")]

for line in plines:
    match = re.match(r"^$", line)

print(puzzle)
ans()
