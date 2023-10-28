from lib import *

input = read_input(2020, 2)

lines = input.splitlines()


out = 0
for policy, char, password in map(str.split, lines):
    char = char[0]
    a, b = map(int, policy.split("-"))
    out += a <= password.count(char) <= b

print(out)


out = 0
for policy, char, password in map(str.split, lines):
    char = char[0]
    a, b = map(int, policy.split("-"))
    out += (password[a - 1] == char) != (password[b - 1] == char)

print(out)
