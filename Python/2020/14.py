from lib import *

input = read_input(2020, 14)

lines = input.splitlines()


mem = {}
mask = "X" * 36
s = 0
for line in lines:
    if match := re.match(r"^mask = ([01X]+)$", line):
        mask = match[1]
    elif match := re.match(r"^mem\[(\d+)\] = (\d+)$", line):
        a, v = map(int, match.groups())
        s -= mem.get(a, 0)
        mem[a] = int("".join(x if m == "X" else m for m, x in zip(mask, bin(v)[2:].zfill(36))), 2)
        s += mem[a]

print(s)


mem = {}
mask = "X" * 36
s = 0
for line in lines:
    if match := re.match(r"^mask = ([01X]+)$", line):
        mask = match[1]
    elif match := re.match(r"^mem\[(\d+)\] = (\d+)$", line):
        a, v = map(int, match.groups())
        a = "".join(x if m == "0" else m for m, x in zip(mask, bin(a)[2:].zfill(36)))
        floating = [i for i in range(36) if a[i] == "X"]
        for i in range(1 << len(floating)):
            ad = [*a]
            for j in range(len(floating)):
                ad[floating[j]] = min((1 << j) & i, 1)
            ad = int("".join(map(str, ad)), 2)
            s -= mem.get(ad, 0)
            mem[ad] = v
            s += mem[ad]

print(s)
