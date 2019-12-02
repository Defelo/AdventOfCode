out = 0
for line in open("input.txt").read().splitlines():
    out += int(line) // 3 - 2
print(out)
