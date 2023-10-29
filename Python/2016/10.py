from lib import *

input = read_input(2016, 10)

values = {}
fw = {}
for line in map(str.split, input.splitlines()):
    if line[0] == "bot":
        src, dst1, dst2 = map(int, [line[1], line[6], line[11]])
        fw[src] = [dst1, dst2]
    else:
        val, dst = map(int, [line[1], line[5]])
        values.setdefault(dst, []).append(val)


queue = []
for k, v in values.items():
    if len(v) == 2:
        queue.append((k, *sorted(v)))

while queue:
    p, l, h = queue.pop()
    if (l, h) == (17, 61):
        print(p)
        break

    values.setdefault(fw[p][0], []).append(l)
    values.setdefault(fw[p][1], []).append(h)

    for k in fw[p]:
        v = values[k]
        if len(v) == 2:
            queue.append((k, *sorted(v)))


values = {}
fw = {}
out = {}
for line in map(str.split, input.splitlines()):
    if line[0] == "bot":
        src, dst1, dst2 = map(int, [line[1], line[6], line[11]])
        fw[src] = [(dst1, line[5] == "output"), (dst2, line[10] == "output")]
    else:
        val, dst = map(int, [line[1], line[5]])
        [values, out][line[4] == "output"].setdefault(dst, []).append(val)


queue = []
for k, v in values.items():
    if len(v) == 2:
        queue.append((k, *sorted(v)))

while queue:
    p, l, h = queue.pop()
    for val, (k, o) in zip([l, h], fw[p]):
        if o:
            out[k] = val
            continue

        values.setdefault(k, []).append(val)
        v = values[k]
        if len(v) == 2:
            queue.append((k, *sorted(v)))

print(out[0] * out[1] * out[2])
