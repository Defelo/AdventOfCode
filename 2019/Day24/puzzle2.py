grid = {0: sum((1<<i*5+j)*(c=="#") for i, line in enumerate(open("input.txt").read().splitlines()) for j, c in enumerate(line))}

def is_alive(i, j, l):
    return grid.get(l, 0)&(1<<i*5+j) > 0

def get_adjacent(i, j, l):
    if (i, j) == (2, 2):
        return 0
    out = 0
    for x, y in [(j-1,i), (j+1,i), (j,i-1), (j,i+1)]:
        outer = grid.get(l - 1, 0)
        layer = grid.get(l, 0)
        inner = grid.get(l + 1, 0)
        if (x, y) == (2, 2):
            for k in range(5):
                if i == 1: out += is_alive(0, k, l + 1)
                elif j == 1: out += is_alive(k, 0, l + 1)
                elif i == 3: out += is_alive(4, k, l + 1)
                elif j == 3: out += is_alive(k, 4, l + 1)
        else:
            if x == -1: out += is_alive(2, 1, l - 1)
            elif y == -1: out += is_alive(1, 2, l - 1)
            elif x == 5: out += is_alive(2, 3, l - 1)
            elif y == 5: out += is_alive(3, 2, l - 1)
            else: out += is_alive(y, x, l)
    return out

def simulate():
    out = {}
    for l in range(min(grid) - 1, max(grid) + 2):
        lout = 0
        for i in range(5):
            for j in range(5):
                if (is_alive(i, j, l) and get_adjacent(i, j, l) == 1) or (not is_alive(i, j, l) and get_adjacent(i, j, l) in (1, 2)):
                    lout |= 1<<i*5+j
        if lout:
            out[l] = lout
    return out

for _ in range(200):
    grid = simulate()
print(sum(bin(layer).count("1") for layer in grid.values()))
