from lib import *

input = read_input(2019, 20)


(*maze,) = map(list, input.splitlines())
hei = len(maze)
wid = len(maze[0])
portals = {}
links = {}
for i in range(2, hei - 2):
    for j in range(2, wid - 2):
        if maze[i][j] != ".":
            continue
        if "A" <= maze[i][j - 1] <= "Z":
            maze[i][j] = (maze[i][j - 2] + maze[i][j - 1]).lower()
        if "A" <= maze[i][j + 1] <= "Z":
            maze[i][j] = (maze[i][j + 1] + maze[i][j + 2]).lower()
        if "A" <= maze[i - 1][j] <= "Z":
            maze[i][j] = (maze[i - 2][j] + maze[i - 1][j]).lower()
        if "A" <= maze[i + 1][j] <= "Z":
            maze[i][j] = (maze[i + 1][j] + maze[i + 2][j]).lower()

        name = maze[i][j]
        if name == ".":
            continue

        if name in portals:
            portals[name].append((i, j))
            links[portals[name][0]] = portals[name][1]
            links[portals[name][1]] = portals[name][0]
        else:
            portals[name] = [(i, j)]

visited = set()
Q = [(0, *portals["aa"][0])]
while Q:
    d, i, j = heapq.heappop(Q)

    if (i, j) in visited:
        continue
    visited.add((i, j))

    if maze[i][j] == "zz":
        print(d)
        break

    if (i, j) in links:
        heapq.heappush(Q, (d + 1, *links[(i, j)]))
    for p, q in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
        if maze[p][q] == "." or "aa" <= maze[p][q] <= "zz":
            heapq.heappush(Q, (d + 1, p, q))

(*maze,) = map(list, input.splitlines())
hei = len(maze)
wid = len(maze[0])
portals = {}
links = {}
for i in range(2, hei - 2):
    for j in range(2, wid - 2):
        if maze[i][j] != ".":
            continue
        if "A" <= maze[i][j - 1] <= "Z":
            maze[i][j] = (maze[i][j - 2] + maze[i][j - 1]).lower()
        if "A" <= maze[i][j + 1] <= "Z":
            maze[i][j] = (maze[i][j + 1] + maze[i][j + 2]).lower()
        if "A" <= maze[i - 1][j] <= "Z":
            maze[i][j] = (maze[i - 2][j] + maze[i - 1][j]).lower()
        if "A" <= maze[i + 1][j] <= "Z":
            maze[i][j] = (maze[i + 1][j] + maze[i + 2][j]).lower()

        name = maze[i][j]
        if name == ".":
            continue

        inner = i not in (2, hei - 3) and j not in (2, wid - 3)
        if name in portals:
            portals[name].append((i, j))
            links[portals[name][0]] = (portals[name][1], 1 - 2 * inner)
            links[portals[name][1]] = (portals[name][0], 2 * inner - 1)
        else:
            portals[name] = [(i, j)]

visited = set()
Q = [(0, *portals["aa"][0], 0)]
while Q:
    d, i, j, m = heapq.heappop(Q)

    if (i, j, m) in visited:
        continue
    visited.add((i, j, m))

    if maze[i][j] == "zz" and m == 0:
        print(d)
        break

    if (i, j) in links:
        link, change = links[(i, j)]
        if m + change >= 0:
            heapq.heappush(Q, (d + 1, *link, m + change))
    for p, q in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
        if maze[p][q] == "." or "aa" <= maze[p][q] <= "zz":
            heapq.heappush(Q, (d + 1, p, q, m))
