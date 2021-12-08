from datetime import date

import aoc
from utils import *  # noqa

year, day = (t := date.today()).year, t.day

puzzle = aoc.setup(year, day, show_title=False)
# puzzle = open("example.txt").read()
plines = puzzle.splitlines()

# nums = [int(x) for x in puzzle.split(",")]

for line in plines:
    match = re.match(r"^$", line)

print(puzzle)
