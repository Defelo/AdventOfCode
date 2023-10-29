from lib import *

input = read_input(2017, 1).strip()


print(sum(int(a) for a, b in zip(input, input[1:] + input[0]) if a == b))


n = len(input) // 2
print(sum(int(a) for a, b in zip(input, input[n:] + input[:n]) if a == b))
