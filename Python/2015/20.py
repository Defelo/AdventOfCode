from lib import *

input = read_input(2015, 20)

target = int(input) // 10

houses = [0] * target
for i in range(1, target + 1):
    k = i - 1
    while k < target:
        houses[k] += i
        k += i

for i, x in enumerate(houses):
    if x >= target:
        print(i + 1)
        break


target = int(input) // 11
houses = [0] * target
for i in range(1, target + 1):
    k = i - 1
    c = 0
    while k < target and c < 50:
        c += 1
        houses[k] += i
        k += i

for i, x in enumerate(houses):
    if x >= target:
        print(i + 1)
        break
