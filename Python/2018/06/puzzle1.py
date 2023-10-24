width = 0
height = 0
coords = []
for line in open("input.txt").read().splitlines():
    x, y = map(int, line.split(", "))
    coords.append((x, y))
    width = max(width, x + 1)
    height = max(height, y + 1)

def find_nearest(x, y):
    distances = [abs(x-p) + abs(y-q) for p, q in coords]
    best_distance = min(distances)
    if distances.count(best_distance) > 1:
        return None
    return distances.index(best_distance)

counter = {}
edge = set()
for y in range(height):
    for x in range(width):
        nearest = find_nearest(x, y)
        if nearest is not None:
            counter[nearest] = counter.get(nearest, 0) + 1
            if x in (0, width - 1) or y in (0, height - 1):
                edge.add(nearest)
print(max(counter[c] for c in counter if c not in edge))
