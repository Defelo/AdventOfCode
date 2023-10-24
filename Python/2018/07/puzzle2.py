import re

unlocks = {}
requirements = {}
chars = set()
for line in open("input.txt").read().splitlines():
    a, b = re.match("^Step (.) must be finished before step (.) can begin\.$", line).groups()
    chars |= {a, b}
    unlocks.setdefault(a, set()).add(b)
    requirements.setdefault(b, set()).add(a)

Q = [e for e in chars if e not in requirements]
seconds = 0
    

def get_task():
    if Q:
        task = Q.pop()
        return (task, ord(task) - 4)

def finish_task(p):
    for q in unlocks.get(p, []):
        requirements[q].remove(p)
        if not requirements[q]:
            Q.append(q)

def fast_forward():
    t = min((w[1] for w in workers if w is not None), default=0)
    for i in range(5):
        if workers[i] is None:
            continue
        workers[i] = workers[i][0], workers[i][1] - t
        if workers[i][1] <= 0:
            finish_task(workers[i][0])
            workers[i] = None
    return t

workers = [None for _ in range(5)]
while Q or any(workers):
    seconds += fast_forward()
    for i in range(5):
        if workers[i] is None:
            workers[i] = get_task()

print(seconds)
