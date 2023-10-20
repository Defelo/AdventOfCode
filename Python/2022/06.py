from lib import *

input = read_input(2022, 6)


for i in range(4, len(input)):
    if len(set(input[i - 4 : i])) == 4:
        print(i)
        break


for i in range(14, len(input)):
    if len(set(input[i - 14 : i])) == 14:
        print(i)
        break
