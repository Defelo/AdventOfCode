from lib import *

input = read_input(2018, 23)

lines = input.splitlines()


bots = []
for pos, r in map(str.split, lines):
    x, y, z = map(int, pos.split("<")[1].strip(">,").split(","))
    r = int(r.split("=")[1])
    bots.append((x, y, z, r))
sx, sy, sz, sr = max(bots, key=lambda a: a[3])

print(sum(abs(x - sx) + abs(y - sy) + abs(z - sz) <= sr for x, y, z, _ in bots))


bots = []
for pos, r in map(str.split, lines):
    x, y, z = map(int, pos.split("<")[1].strip(">,").split(","))
    r = int(r.split("=")[1])
    bots.append((x, y, z, r))

zabs = lambda x: z3.If(x >= 0, x, -x)
x, y, z = z3.Ints("x y z")
in_ranges = [z3.Int(f"in_range_{i}") for i in range(len(bots))]
range_count = z3.Int("sum")
o = z3.Optimize()
for (nx, ny, nz, nr), ir in zip(bots, in_ranges):
    o.add(ir == z3.If(zabs(x - nx) + zabs(y - ny) + zabs(z - nz) <= nr, 1, 0))

o.add(range_count == sum(in_ranges))
dist_from_zero = z3.Int("dist")
o.add(dist_from_zero == zabs(x) + zabs(y) + zabs(z))
h1 = o.maximize(range_count)
h2 = o.minimize(dist_from_zero)
o.check()

print(o.lower(h2))
