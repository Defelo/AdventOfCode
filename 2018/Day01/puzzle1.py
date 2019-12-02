out = 0
for line in open("input.txt").read().splitlines():
    out += int(line)
print(out)
