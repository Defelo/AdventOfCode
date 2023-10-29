from lib import *

input = read_input(2016, 14).strip()


def hash(idx):
    return hashlib.md5(f"{input}{idx}".encode()).hexdigest()


def first(idx):
    if match := re.match(r"^.*?((.)\2{2}).*$", hash(idx)):
        return match.group(2)


def second(idx, m):
    for i in range(1, 1001):
        if m * 5 in hash(idx + i):
            return True

    return False


idx = 0
cnt = 0
while True:
    if (x := first(idx)) and second(idx, x):
        cnt += 1
    if cnt == 64:
        break
    idx += 1

print(idx)


dp = {}


def hash(idx):
    if idx in dp:
        return dp[idx]

    out = hashlib.md5(f"{input}{idx}".encode()).hexdigest()

    for _ in range(2016):
        out = hashlib.md5(out.encode()).hexdigest()

    dp[idx] = out

    return out


def first(idx):
    if match := re.match(r"^.*?((.)\2{2}).*$", hash(idx)):
        return match.group(2)


def second(idx, m):
    for i in range(1, 1001):
        if m * 5 in hash(idx + i):
            return True

    return False


idx = 0
cnt = 0
while True:
    if (x := first(idx)) and second(idx, x):
        cnt += 1
    if cnt == 64:
        break
    idx += 1

print(idx)
