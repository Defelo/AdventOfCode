wid = 25
hei = 6

layers = []
for i, c in enumerate(map(int, open("input.txt").read().strip())):
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
    print("".join(" #"[d]*2 for d in l))
