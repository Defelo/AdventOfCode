from lib import *

input = read_input(2015, 4)


def check(inp, cnt):
    return hashlib.md5(inp.encode()).hexdigest().startswith("0" * cnt)


def mine(cnt):
    i = 1
    while not check(f"{input.strip()}{i}", cnt):
        i += 1
    return i


print(mine(5))
print(mine(6))
