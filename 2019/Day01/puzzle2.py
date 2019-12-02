out = 0
for line in open("input.txt").read().splitlines():
    fuel = int(line) // 3 - 2
    while fuel > 0:
        out += fuel
        fuel = fuel // 3 - 2
print(out)
