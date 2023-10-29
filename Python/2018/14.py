from lib import *

input = read_input(2018, 14)


r = [3, 7]
p = 0
q = 1
n = int(input)
while len(r) < n + 10:
    r += [*map(int, str(r[p] + r[q]))]
    p = (p + r[p] + 1) % len(r)
    q = (q + r[q] + 1) % len(r)
print("".join(map(str, r[n : n + 10])))


def gen():
    r = [3, 7]
    p = 0
    q = 1
    yield from r
    while True:
        x = [*map(int, str(r[p] + r[q]))]
        yield from x
        r += x
        p = (p + r[p] + 1) % len(r)
        q = (q + r[q] + 1) % len(r)


n = [*map(int, input.strip())]
g = gen()
lst = [i for _, i in zip(n, g)]
cnt = 0
for num in g:
    if lst == n:
        print(cnt)
        break
    lst.pop(0)
    lst.append(num)
    cnt += 1
