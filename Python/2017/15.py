from lib import *

input = read_input(2017, 15)

lines = input.splitlines()


def gen(prev, k):
    return (prev * k) % 2147483647


a, b = [int(line.split()[-1]) for line in lines]
out = 0
M = 1 << 16
for _ in range(40000000):
    a = gen(a, 16807)
    b = gen(b, 48271)
    out += a % M == b % M

print(out)


def generate(x, k, m):
    while True:
        x = gen(x, k)
        if x % m == 0:
            yield x


a, b = [int(line.split()[-1]) for line in lines]
out = 0
M = 1 << 16
for _, a, b in zip(range(5000000), generate(a, 16807, 4), generate(b, 48271, 8)):
    out += a % M == b % M
print(out)
