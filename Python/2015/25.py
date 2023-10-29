from lib import *

input = read_input(2015, 25)

row, col = map(int, re.match(r"^.*?(\d+).*?(\d+).*?$", input).groups())


n = row + col - 2
print((20151125 * pow(252533, n * (n + 1) // 2 + 1 + col - 2, 33554393)) % 33554393)
