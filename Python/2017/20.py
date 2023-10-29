from lib import *

input = read_input(2017, 20)

lines = input.splitlines()


n = 0
p = []
v = []
a = []
for line in lines:
    x, y, z = [tuple(map(int, k.split("<")[1].strip(">").split(","))) for k in line.split(", ")]
    p.append(x)
    v.append(y)
    a.append(z)
    n += 1

for _ in range(2000):
    for i in range(n):
        v[i] = v[i][0] + a[i][0], v[i][1] + a[i][1], v[i][2] + a[i][2]
        p[i] = v[i][0] + p[i][0], v[i][1] + p[i][1], v[i][2] + p[i][2]

print(min(range(n), key=lambda i: sum(map(abs, p[i]))))


p = []
v = []
a = []
for line in lines:
    x, y, z = [tuple(map(int, k.split("<")[1].strip(">").split(","))) for k in line.split(", ")]
    p.append(x)
    v.append(y)
    a.append(z)

pos = lambda i, t: tuple(p[i][k] + t * v[i][k] + t * (t + 1) // 2 * a[i][k] for k in range(3))
alive = set(range(len(p)))
for t in range(1000):
    s = {}
    for i in alive:
        s.setdefault(pos(i, t), []).append(i)
    for y, x in s.items():
        if len(x) > 1:
            alive.difference_update(x)

print(len(alive))
