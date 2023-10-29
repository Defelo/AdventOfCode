from lib import *

input = read_input(2015, 8)

print(sum(len(line) - len(eval(line)) for line in input.splitlines()))
print(sum(len(repr(line)) + line.count('"') - len(line) for line in input.splitlines()))
