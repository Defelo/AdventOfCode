from collections import deque
import re

players, n = map(int, re.match(r"^(\d+) players; last marble is worth (\d+) points$", open("input.txt").read()).groups())
lst = deque([0])
scores = [0 for _ in range(players)]
for i in range(1, n + 1):
    p = i % players
    if i % 23:
        lst.rotate(-1)
        lst.append(i)
    else:
        lst.rotate(7)
        scores[p] += i + lst.pop()
        lst.rotate(-1)
print(max(scores))
