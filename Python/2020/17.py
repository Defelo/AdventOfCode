from lib import *

input = read_input(2020, 17)

lines = input.splitlines()

neigh = [(i, j, k) for i in [-1, 0, 1] for j in [-1, 0, 1] for k in [-1, 0, 1]]


state = {(0, i, j) for i, line in enumerate(lines) for j, c in enumerate(line) if c == "#"}
for _ in range(6):
    to_update = {(x + i, y + j, z + k) for (x, y, z) in state for (i, j, k) in neigh}
    new_state = set()
    for x, y, z in to_update:
        active = (x, y, z) in state
        cnt = sum((x + i, y + j, z + k) in state for (i, j, k) in neigh if i or j or k)
        if active and cnt not in (2, 3):
            active = False
        elif not active and cnt == 3:
            active = True
        if active:
            new_state.add((x, y, z))
    state = new_state

print(len(state))


neigh = [(i, j, k, l) for i in [-1, 0, 1] for j in [-1, 0, 1] for k in [-1, 0, 1] for l in [-1, 0, 1]]


state = {(0, 0, i, j) for i, line in enumerate(lines) for j, c in enumerate(line) if c == "#"}
for _ in range(6):
    to_update = {(w + h, x + i, y + j, z + k) for (w, x, y, z) in state for (h, i, j, k) in neigh}
    new_state = set()
    for w, x, y, z in to_update:
        active = (w, x, y, z) in state
        cnt = sum((w + h, x + i, y + j, z + k) in state for (h, i, j, k) in neigh if h or i or j or k)
        if active and cnt not in (2, 3):
            active = False
        elif not active and cnt == 3:
            active = True
        if active:
            new_state.add((w, x, y, z))
    state = new_state

print(len(state))
