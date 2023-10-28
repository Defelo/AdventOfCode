from lib import *

input = read_input(2020, 7)

lines = input.splitlines()


g = {}
for line in lines:
    a, *b = re.findall(r"([a-z]+ [a-z]+) bags?", line)
    for x in b:
        g.setdefault(x, []).append(a)

cnt = -1
q = ["shiny gold"]
visited = set()
while q:
    p = q.pop(0)

    if p in visited:
        continue
    visited.add(p)

    cnt += 1
    q += g.get(p, [])

print(cnt)


g = {}
for line in lines:
    a = re.match(r"^([a-z]+ [a-z]+) bags", line).group(1)
    b = re.findall(r"(\d+) ([a-z]+ [a-z]+) bags?", line)
    g.setdefault(a, []).extend(b)

cnt = -1
q = [(1, "shiny gold")]
while q:
    n, p = q.pop(0)
    cnt += n
    q += [(n * int(a), b) for a, b in g.get(p, [])]

print(cnt)
