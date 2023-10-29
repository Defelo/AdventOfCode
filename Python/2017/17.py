from lib import *

input = read_input(2017, 17)


n = int(input)
lst = [0]
pos = 0
for i in range(1, 2018):
    pos = (pos + n) % len(lst) + 1
    lst.insert(pos, i)
print(lst[(lst.index(2017) + 1) % len(lst)])


k = int(input)
i = 0
last = None
for j in range(1, 50000000 + 1):
    i = (i + k) % j + 1
    if i == 1:
        last = j
print(last)
