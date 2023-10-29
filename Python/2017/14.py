from lib import *

input = read_input(2017, 14).strip()


def knot(inp):
    lengths = [*map(ord, inp), 17, 31, 73, 47, 23]
    nums = [*range(256)]
    pos = 0
    skip = 0

    def rev(a, b):
        a %= len(nums)
        b = (b - a) % len(nums)
        nums[:] = nums[a:] + nums[:a]
        nums[:b] = nums[:b][::-1]
        nums[:] = nums[-a:] + nums[:-a]

    for _ in range(64):
        for length in lengths:
            rev(pos, pos + length)
            pos += length + skip
            skip += 1

    dense = []
    for i in range(16):
        x = 0
        for j in range(16):
            x ^= nums[i * 16 + j]
        dense.append(x)
    return "".join(f"{x:02x}" for x in dense)


out = 0
for i in range(128):
    k = bin(int(knot(f"{input}-{i}"), 16))[2:].zfill(128)
    out += k.count("1")

print(out)


last = None
uf = UnionFind(128 * 128)
free = 0
for i in range(128):
    k = bin(int(knot(f"{input}-{i}"), 16))[2:].zfill(128)
    for j in range(128):
        if k[j] != "1":
            free += 1
            continue
        if i and last[j] == "1":
            uf.merge((i - 1) * 128 + j, i * 128 + j)
        if j and k[j - 1] == "1":
            uf.merge(i * 128 + j - 1, i * 128 + j)
    last = k

groups = set()
for i in range(128 * 128):
    groups.add(uf.find(i))
print(len(groups) - free)
