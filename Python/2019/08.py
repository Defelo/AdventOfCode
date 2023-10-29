from lib import *

input = read_input(2019, 8)

wid = 25
hei = 6

layers = []
for i, c in enumerate(map(int, input.strip())):
    if i % (wid * hei) == 0:
        layers.append([])
    if i % wid == 0:
        layers[-1].append([])
    layers[-1][-1].append(c)


def count(layer, digit):
    return sum(d == digit for l in layer for d in l)


fewest = wid * hei
out = 0
for layer in layers:
    cnt = count(layer, 0)
    if cnt < fewest:
        fewest = cnt
        out = count(layer, 1) * count(layer, 2)
print(out)


wid = 25
hei = 6

layers = []
for i, c in enumerate(map(int, input.strip())):
    if i % (wid * hei) == 0:
        layers.append([])
    if i % wid == 0:
        layers[-1].append([])
    layers[-1][-1].append(c)

out = []
for i in range(hei):
    out.append([])
    for j in range(wid):
        d = 2
        for layer in layers:
            if layer[i][j] != 2:
                d = layer[i][j]
                break
        out[-1].append(d)
for l in out:
    print("".join(" #"[d] * 2 for d in l))
