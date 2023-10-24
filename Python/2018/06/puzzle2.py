width = 0
height = 0
coords = []
for line in open("input.txt").read().splitlines():
    x, y = map(int, line.split(", "))
    coords.append((x, y))
    width = max(width, x + 1)
    height = max(height, y + 1)

def measure(x, y):
    return sum(abs(x-p) + abs(y-q) for p, q in coords)

print(sum(measure(x, y) < 10000 for x in range(width) for y in range(height)))
