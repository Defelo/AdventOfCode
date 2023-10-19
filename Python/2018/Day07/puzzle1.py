import heapq
import re

unlocks = {}
requirements = {}
chars = set()
for line in open("input.txt").read().splitlines():
    a, b = re.match("^Step (.) must be finished before step (.) can begin\.$", line).groups()
    chars |= {a, b}
    unlocks.setdefault(a, set()).add(b)
    requirements.setdefault(b, set()).add(a)

out = ""
Q = [e for e in chars if e not in requirements]
heapq.heapify(Q)
while Q:
    p = heapq.heappop(Q)
    out += p
    
    for q in unlocks.get(p, []):
        requirements[q].remove(p)
        if not requirements[q]:
            heapq.heappush(Q, q)
print(out)
