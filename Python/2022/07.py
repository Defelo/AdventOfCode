from lib import *

input = read_input(2022, 7)

sizes = {}
seen = set()
pwd = ()
for line in input.splitlines():
    if match := re.match(r"^\$ cd (.+)$", line):
        d = match[1]
        if d == "/":
            pwd = ()
        elif d == "..":
            pwd = pwd[:-1]
        else:
            pwd = (*pwd, d)
    elif match := re.match(r"^(\d+) (.+)$", line):
        s = int(match[1])
        n = match[2]

        if (pwd, n) in seen:
            continue
        seen.add((pwd, n))

        d = pwd
        while True:
            sizes[d] = sizes.get(d, 0) + s
            if not d:
                break
            d = d[:-1]


print(sum(s for s in sizes.values() if s <= 100000))

free = 70000000 - sizes[()]
print(min(s for s in sizes.values() if s >= 30000000 - free))
