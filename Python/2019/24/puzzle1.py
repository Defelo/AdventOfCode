grid = sum((1<<i*5+j)*(c=="#") for i, line in enumerate(open("input.txt").read().splitlines()) for j, c in enumerate(line))

def get_adjacent(i, j):
    return sum(grid&(1<<i*5+j)>0 for i, j in [(i,j-1), (i,j+1), (i-1,j), (i+1,j)] if i in range(5) and j in range(5))

def simulate():
    out = 0
    for i in range(5):
        for j in range(5):
            if (grid&(1<<i*5+j)>0 and get_adjacent(i, j) == 1) or (grid&(1<<i*5+j)==0 and get_adjacent(i, j) in (1, 2)):
                out |= 1<<i*5+j
    return out

history = set()
while True:
    if grid in history:
        break
    history.add(grid)
    grid = simulate()
print(grid)
