from lib import *

input = read_input(2017, 25)

lines = input.splitlines()


state = lines[0].split()[-1].strip(".")
n = int(lines[1].split()[-2])
tm = {}
on = set()
p = 0
for s in map(str.splitlines, input.split("\n\n")[1:]):
    tm[s[0].split()[-1].strip(":")] = [
        (
            int(s[i * 4 + 2].split()[-1].strip(".")),
            -1 if s[i * 4 + 3].split()[-1] == "left." else 1,
            s[i * 4 + 4].split()[-1].strip("."),
        )
        for i in range(2)
    ]

for _ in range(n):
    s = tm[state][p in on]
    if s[0]:
        on.add(p)
    else:
        on.discard(p)
    p += s[1]
    state = s[2]

print(len(on))
