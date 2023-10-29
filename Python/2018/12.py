from lib import *

input = read_input(2018, 12)

lines = input.splitlines()


state, _, *rules = lines
state = {i for i, x in enumerate(state.split()[-1]) if x == "#"}
rules = [x.split()[-1] == "#" for x in sorted(rules, reverse=True)]
for _ in range(20):
    nums = {k + i - 2 for k in state for i in range(5)}
    state = {k for k in nums if rules[sum(((k + i - 2) in state) * (2 ** (4 - i)) for i in range(5))]}

print(sum(state))


state, _, *rules = lines
state = {i for i, x in enumerate(state.split()[-1]) if x == "#"}
rules = [x.split()[-1] == "#" for x in sorted(rules, reverse=True)]
last = None
for _ in range(200):
    nums = {k + i - 2 for k in state for i in range(5)}
    state = {k for k in nums if rules[sum(((k + i - 2) in state) * (2 ** (4 - i)) for i in range(5))]}
    s = sum(state)
    step = s - last if last is not None else None
    last = s

print(s + step * (50000000000 - 200))
