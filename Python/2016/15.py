from lib import *

input = read_input(2016, 15)

discs = []

for line in input.splitlines():
    words = line.split()
    discs.append((int(words[3]), int(words[-1][:-1])))


def test(t):
    return all((t + x + i + 1) % n == 0 for i, (n, x) in enumerate(discs))


t = 0

while not test(t):
    t += 1

print(t)


discs = []
for line in input.splitlines():
    words = line.split()
    discs.append((int(words[3]), int(words[-1][:-1])))

discs.append((11, 0))

t = 0
while not test(t):
    t += 1

print(t)
