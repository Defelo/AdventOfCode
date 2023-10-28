from lib import *

input = read_input(2021, 17)


match = re.match(r"^target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)$", input)
y1 = int(match.group(3))
print(y1 * (y1 + 1) // 2)


x1, x2, y1, y2 = map(int, match.groups())
ok = {}
maxt = 0
for vy in range(y1, -y1 + 1):
    ovy = vy
    t = 0
    y = 0
    while y >= y1:
        if y1 <= y <= y2:
            ok.setdefault(t, set()).add(ovy)
            maxt = max(t, maxt)
        y += vy
        vy -= 1
        t += 1

out = 0
for vx in range(x2 + 1):
    if (vx + 1) ** 2 - (vx + 1) * (vx + 2) // 2 < x1:
        continue

    t = 0
    x = 0
    found = set()
    while x <= x2 and t <= maxt:
        if x1 <= x <= x2:
            found.update(ok.get(t, []))
        x += vx
        vx = max(vx - 1, 0)
        t += 1
    out += len(found)

print(out)
