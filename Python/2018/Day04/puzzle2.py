from datetime import date, timedelta
import re

days = {}
for line in open("input.txt").read().splitlines():
    day, hour, minute, action = re.match("^\[(\d\d\d\d-\d\d-\d\d) (\d\d):(\d\d)\] (.*)$", line).groups()
    day = date.fromisoformat(day)
    hour, minute = int(hour), int(minute)
    match = re.match("^Guard #(\d+) begins shift$", action)
    if match:
        guard = int(match.group(1))
        if hour == 23:
            day += timedelta(days=1)
        days.setdefault(day, [None, [None for _ in range(60)]])[0] = guard
    elif action == "wakes up":
        assert hour == 0
        assert 0 <= minute < 60
        days.setdefault(day, [None, [None for _ in range(60)]])[1][minute] = True
    elif action == "falls asleep":
        assert hour == 0
        assert 0 <= minute < 60
        days.setdefault(day, [None, [None for _ in range(60)]])[1][minute] = False
    else:
        assert False
guards = {}
for day, (guard, logs) in days.items():
    awake = True
    for i in range(60):
        if logs[i] is None:
            logs[i] = awake
        else:
            awake = logs[i]
        if not awake:
            guards.setdefault(guard, [0, {}])[0] += 1
            guards[guard][1][i] = guards[guard][1].get(i, 0) + 1
best_cnt = -1
out = None
for g in guards:
    cnt, best_minute = max((guards[g][1].get(i, 0), i) for i in range(60)) 
    if cnt > best_cnt:
        best_cnt = cnt
        out = best_minute * g
print(out)
