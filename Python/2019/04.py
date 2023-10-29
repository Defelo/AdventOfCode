from lib import *

input = read_input(2019, 4)

a, b = map(int, input.split("-"))
count = 0
for i in range(a, b + 1):
    string = str(i)
    if list(string) != sorted(string):
        continue
    if any(x == y for x, y in zip(string[1:], string)):
        count += 1
print(count)


a, b = map(int, input.split("-"))
count = 0
for i in range(a, b + 1):
    string = str(i)
    if list(string) != sorted(string):
        continue
    if any(string.count(c) == 2 for c in string):
        count += 1
print(count)
