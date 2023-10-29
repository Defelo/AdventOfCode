from lib import *

input = read_input(2016, 5)


out = ""
i = 0
while len(out) < 8:
    while not (digest := hashlib.md5(f"{input.strip()}{i}".encode()).hexdigest()).startswith("0" * 5):
        i += 1
    out += digest[5]
    i += 1

print(out)


out = ["_"] * 8
i = 0
while "_" in out:
    while not (digest := hashlib.md5(f"{input.strip()}{i}".encode()).hexdigest()).startswith("0" * 5):
        i += 1
    i += 1
    if not "0" <= digest[5] <= "7" or out[(d := int(digest[5]))] != "_":
        continue
    out[d] = digest[6]

print("".join(out))
