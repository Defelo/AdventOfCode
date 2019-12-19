from string import ascii_lowercase as letters
import heapq

KEY_INDEX = {a: i for i, a in enumerate(letters)}
def key_to_num(key):
    return 1 << KEY_INDEX[key.lower()]

grid = []
pos = None
key_count = 0
graph = {str(i): {} for i in range(4)}
nodes = {str(i): None for i in range(4)}
for i, line in enumerate(open("input.txt").read().splitlines()):
    line = list(line)
    for j, c in enumerate(line):
        if "a" <= c <= "z":
            key_count += 1
            graph[c] = {}
            nodes[c] = j, i
        elif "A" <= c <= "Z":
            graph[c] = {}
            nodes[c] = j, i
        elif c == "@":
            pos = j, i
            graph[c] = {}
            nodes[c] = j, i
    grid.append(line)

sector_keys = [
    set(grid[i][j] for i in range(len(grid) // 2) for j in range(len(grid[0]) // 2) if "a" <= grid[i][j] <= "z"),
    set(grid[i][j] for i in range(len(grid) // 2) for j in range(len(grid[0]) // 2, len(grid[0])) if "a" <= grid[i][j] <= "z"),
    set(grid[i][j] for i in range(len(grid) // 2, len(grid)) for j in range(len(grid[0]) // 2) if "a" <= grid[i][j] <= "z"),
    set(grid[i][j] for i in range(len(grid) // 2, len(grid)) for j in range(len(grid[0]) // 2, len(grid[0])) if "a" <= grid[i][j] <= "z")
]

j, i = pos
del graph["@"]
del nodes["@"]
for i, j in [(i, j), (i-1, j), (i+1, j), (i, j-1), (i, j+1)]:
    grid[i][j] = "#"
j, i = pos
grid[i-1][j-1] = "0"
grid[i-1][j+1] = "1"
grid[i+1][j-1] = "2"
grid[i+1][j+1] = "3"
nodes["0"] = j-1, i-1
nodes["1"] = j+1, i-1
nodes["2"] = j-1, i+1
nodes["3"] = j+1, i+1

for src in graph:
    Q = [(*nodes[src], 0)]
    visited = set()
    while Q:
        x, y, d = Q.pop(0)
        
        if (x, y) in visited:
            continue
        visited.add((x, y))

        cell = grid[y][x]
        if cell != src and cell in graph:
            graph[src][cell] = min(d, graph[src].get(cell, 1e1337))
            continue

        for dx, dy in [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]:
            if not (dx in range(len(grid[0])) and dy in range(len(grid))):
                continue
            if grid[dy][dx] == "#":
                continue
            Q.append((dx, dy, d + 1))

out = 0
for robot in range(4):
    visited = set()
    Q = [(0, str(robot), 0)]
    while Q:
        d, p, keys = heapq.heappop(Q)

        if (p, keys) in visited:
            continue
        visited.add((p, keys))

        if keys == sum(key_to_num(ek) for ek in sector_keys[robot]):
            out += d
            break

        j, i = nodes[p]
        cell = grid[i][j]
        if "a" <= cell <= "z" and not key_to_num(cell) & keys:
            heapq.heappush(Q, (d, p, keys | key_to_num(cell)))
            continue

        for q, dist in graph[p].items():
            if "A" <= q <= "Z" and not key_to_num(q.lower()) & keys and q in sector_keys[robot]:
                continue
            heapq.heappush(Q, (d + dist, q, keys))
print(out)
