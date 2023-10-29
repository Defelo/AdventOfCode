from lib import *

input = read_input(2015, 16)

aunts = [
    {k: int(v) for k, v in map(str.split, "".join(line.split()[2:]).replace(":", " ").split(","))}
    for line in input.splitlines()
]


sue = {
    "children": 3,
    "cats": 7,
    "samoyeds": 2,
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1,
}


for i, x in enumerate(aunts):
    if all(k not in x or x[k] == v for k, v in sue.items()):
        print(i + 1)
        break


for i, x in enumerate(aunts):
    if not all(k not in x or x[k] > sue[k] for k in ["cats", "trees"]):
        continue
    if not all(k not in x or x[k] < sue[k] for k in ["pomeranians", "goldfish"]):
        continue
    if all(k not in x or x[k] == v for k, v in sue.items() if k not in ["cats", "trees", "pomeranians", "goldfish"]):
        print(i + 1)
        break
