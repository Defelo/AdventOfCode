wid = 25
hei = 6

layers = []
for i, c in enumerate(map(int, open("input.txt").read().strip())):
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
