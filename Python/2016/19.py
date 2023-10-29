from lib import *

input = read_input(2016, 19)

n = int(input)

print((n - (1 << n.bit_length() - 1) << 1) + 1)

n = int(input)
l = int(math.log(n, 3))
x = 3**l + 1
y = 2 * 3**l
z = 3 ** (l + 1)
if n <= y:
    print(n - x + 1)
else:
    print(n * 2 - y - x + 1)
