from lib import *

input = read_input(2018, 20)


def parse_regex(regex):
    regex = regex.strip("^$")
    stack = [[]]
    for c in regex:
        if c == "(":
            stack.append([])
            stack.append([])
        elif c == ")":
            x = stack.pop()
            y = stack.pop()
            y.append(x)
            stack[-1].append(tuple(y))
        elif c == "|":
            x = stack.pop()
            stack[-1].append(x)
            stack.append([])
        else:
            stack[-1].append(c)
    return stack[0]


dp = set()


def traverse(graph, regex, x, y, decisions=None, pop_after=None):
    decisions = decisions or ()
    if (x, y, decisions) in dp:
        return

    dp.add((x, y, decisions))
    i = 0
    while i < len(regex):
        if i == pop_after:
            decisions = (*decisions[:-1], None)

        elem = regex[i]
        prev = x, y
        if isinstance(elem, tuple):
            for j, option in enumerate(elem):
                traverse(graph, option + regex[i + 1 :], x, y, (*decisions, j), len(option))
            break

        elif elem == "N":
            y -= 1
        elif elem == "E":
            x += 1
        elif elem == "S":
            y += 1
        elif elem == "W":
            x -= 1
        graph.setdefault(prev, set()).add((x, y))
        graph.setdefault((x, y), set()).add(prev)
        i += 1


regex = parse_regex(input)
graph = {}
traverse(graph, regex, 0, 0)
queue = [(0, 0, 0)]
visited = set()
out = 0
while queue:
    d, x, y = queue.pop(0)
    if (x, y) in visited:
        continue

    visited.add((x, y))
    out = max(out, d)
    for p, q in graph.get((x, y), []):
        if (p, q) not in visited:
            queue.append((d + 1, p, q))

print(out)


dp.clear()
regex = parse_regex(input)
graph = {}
traverse(graph, regex, 0, 0)
queue = [(0, 0, 0)]
visited = set()
out = 0
while queue:
    d, x, y = queue.pop(0)
    if (x, y) in visited:
        continue

    visited.add((x, y))
    if d >= 1000:
        out += 1

    for p, q in graph.get((x, y), []):
        if (p, q) not in visited:
            queue.append((d + 1, p, q))

print(out)
