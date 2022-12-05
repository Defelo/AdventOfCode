from datetime import date
import sys

import aoc
from utils.live import *

year, day = (t := date.today()).year, t.day

puzzle = aoc.setup(year, day, strip="\n", f=sys.argv[1] if len(sys.argv) > 1 else "input.txt")
plines = puzzle.splitlines()

# nums = ints(puzzle)
# nums = [int(x) for x in puzzle.split(",")]

out = 0
for line in plines:
    match = re.match(r"^$", line)

# print(puzzle)
ans(out)
