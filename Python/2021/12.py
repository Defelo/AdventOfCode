from lib import *

input = read_input(2021, 12)

lines = input.splitlines()

edges = {}
for line in lines:
    a, b = line.split("-")
    edges.setdefault(a, []).append(b)
    edges.setdefault(b, []).append(a)


def search1(node, visited):
    if node == "end":
        return 1
    if node.islower() and node in visited:
        return 0

    visited.add(node)
    out = sum(search1(n, visited) for n in edges.get(node, []))
    visited.discard(node)
    return out


print(search1("start", set()))


edges = {}
for line in lines:
    a, b = line.split("-")
    edges.setdefault(a, []).append(b)
    edges.setdefault(b, []).append(a)


def search2(node, visited, twice):
    if node == "end":
        return 1

    tw = False
    if node.islower() and node in visited:
        if twice or node == "start":
            return 0
        tw = True

    visited.add(node)
    out = sum(search2(n, visited, twice or tw) for n in edges.get(node, []))
    if not tw:
        visited.discard(node)

    return out


print(search2("start", set(), False))
