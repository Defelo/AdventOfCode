from lib import *

input = read_input(2021, 22)

lines = input.splitlines()


def line_intersection(a1, a2, b1, b2):
    if a2 < b1 or b2 < a1:
        return None

    return max(a1, b1), min(a2, b2)


@dataclass
class Cuboid:
    x1: int
    x2: int
    y1: int
    y2: int
    z1: int
    z2: int
    off = None

    def get_intersection(self, other):
        x = line_intersection(self.x1, self.x2, other.x1, other.x2)

        y = line_intersection(self.y1, self.y2, other.y1, other.y2)

        z = line_intersection(self.z1, self.z2, other.z1, other.z2)

        if None in [x, y, z]:
            return None

        return Cuboid(*x, *y, *z)

    def subtract(self, other):
        if intersection := self.get_intersection(other):
            if not self.off:
                self.off = []

            for o in self.off:
                o.subtract(other)

            self.off.append(intersection)

    def volume(self):
        return (self.x2 - self.x1 + 1) * (self.y2 - self.y1 + 1) * (self.z2 - self.z1 + 1) - sum(
            o.volume() for o in self.off or []
        )


cuboids = []
for line in lines:
    match = re.match(r"^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)$", line)
    if any(x not in range(-50, 51) for x in map(int, match.groups()[1:])):
        continue

    on = match.group(1) == "on"
    cuboid = Cuboid(*map(int, match.groups()[1:]))

    for c in cuboids:
        c.subtract(cuboid)
    if on:
        cuboids.append(cuboid)

print(sum(c.volume() for c in cuboids))


cuboids = []
for line in lines:
    match = re.match(r"^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)$", line)
    on = match.group(1) == "on"
    cuboid = Cuboid(*map(int, match.groups()[1:]))
    for c in cuboids:
        c.subtract(cuboid)
    if on:
        cuboids.append(cuboid)

print(sum(c.volume() for c in cuboids))
