from lib import *

input = read_input(2016, 13)

num = int(input)

wall = lambda x, y: bin(x * x + 3 * x + 2 * x * y + y + y * y + num).count("1") % 2

q = [(0, 1, 1)]
visited = set()
while q:
    d, x, y = q.pop(0)
    if (x, y) in visited:
        continue
    visited.add((x, y))
    if (x, y) == (31, 39):
        print(d)
        break
    for nx, ny in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
        if nx < 0 or ny < 0 or wall(nx, ny):
            continue
        q.append((d + 1, nx, ny))

q = [(0, 1, 1)]
visited = set()
count = 0
while q:
    d, x, y = q.pop(0)
    if (x, y) in visited:
        continue
    visited.add((x, y))
    if d > 50:
        break
    count += 1
    for nx, ny in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
        if nx < 0 or ny < 0 or wall(nx, ny):
            continue
        q.append((d + 1, nx, ny))
print(count)
