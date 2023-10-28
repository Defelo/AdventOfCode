from lib import *

input = read_input(2021, 19)

ROT1 = [
    np.array([[1, 0, 0], [0, 1, 0], [0, 0, 1]]),
    np.array([[-1, 0, 0], [0, -1, 0], [0, 0, 1]]),
    np.array([[0, 1, 0], [-1, 0, 0], [0, 0, 1]]),
    np.array([[0, -1, 0], [1, 0, 0], [0, 0, 1]]),
    np.array([[0, 0, 1], [0, -1, 0], [1, 0, 0]]),
    np.array([[0, 0, -1], [0, 1, 0], [1, 0, 0]]),
]


ROT2 = [
    np.array([[1, 0, 0], [0, 1, 0], [0, 0, 1]]),
    np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]]),
    np.array([[1, 0, 0], [0, -1, 0], [0, 0, -1]]),
    np.array([[1, 0, 0], [0, 0, 1], [0, -1, 0]]),
]


def rotations():
    for a in ROT1:
        for b in ROT2:
            yield lambda x: b @ (a @ x)


def match_scanner(beacons, scanner_positions, scanner):
    for rot in rotations():
        counter = Counter()

        for rel in map(rot, scanner[0]):
            for abs_ in map(np.array, beacons):
                k = abs_ - rel

                counter[kt := tuple(k)] += 1

                if counter[kt] >= 12:
                    beacons.update([tuple(rot(x) + k) for x in scanner[0]])

                    scanner_positions.append(k)

                    return


remaining = []
for block in input.split("\n\n"):
    positions = [np.array(tuple(map(int, line.split(",")))) for line in block.splitlines()[1:]]
    distances = {int((x := a - b).dot(x)) for a in positions for b in positions}
    remaining.append((positions, distances))

first = remaining.pop(0)
beacons = {*map(tuple, first[0])}
distances = first[1].copy()
scanners = []
while remaining:
    i = max(range(len(remaining)), key=lambda j: len(remaining[j][1] & distances))
    s = remaining.pop(i)

    match_scanner(beacons, scanners, s)

    distances.update(s[1])


print(len(beacons))
print(max(sum(np.abs(a - b)) for a in scanners for b in scanners))
