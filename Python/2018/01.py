from lib import *

input = read_input(2018, 1)

out = 0
for line in input.splitlines():
    out += int(line)
print(out)

lines = input.splitlines()
freq = 0
seen = {0}
while True:
    for line in lines:
        freq += int(line)
        if freq in seen:
            print(freq)
            break
        else:
            seen.add(freq)
    else:
        continue
    break
