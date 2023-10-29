from lib import *

input = read_input(2017, 13)

lines = input.splitlines()


R = {}
S = {}
for line in lines:
    a, b = map(int, line.split(": "))
    R[a] = b
    S[a] = 0

n = max(S)
out = 0
for i in range(n):
    if i in S and S[i] == 0:
        out += i * R[i]

    for k, v in S.items():
        S[k] = (v + 1) % (R[k] * 2 - 2)

print(out)


def step(R, S):
    for k, v in S.items():
        S[k] = (v + 1) % (R[k] * 2 - 2)


R = {}
SC = {}
for line in lines:
    a, b = map(int, line.split(": "))
    R[a] = b
    SC[a] = 0

n = max(SC)


def test():
    S = SC.copy()
    for i in range(n + 1):
        if i in S and S[i] == 0:
            return False
        step(R, S)
    return True


k = 0
while not test():
    k += 1
    step(R, SC)

print(k)
