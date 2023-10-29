from lib import *

input = read_input(2019, 16)

(*lst,) = map(int, input.strip())
n = len(lst)

for _ in range(100):
    out = []
    for i in range(n):
        j = i
        s = 0
        while j < n:
            for _ in range(i + 1):
                if j >= n:
                    break
                s += lst[j]
                j += 1
            j += i + 1

            for _ in range(i + 1):
                if j >= n:
                    break
                s -= lst[j]
                j += 1
            j += i + 1
        out.append(abs(s) % 10)
    lst = out

print("".join(map(str, lst[:8])))


(*lst,) = map(int, input.strip())
offset = int("".join(map(str, lst[:7])))
lst = (lst * 10_000)[: offset - 1 : -1]
n = len(lst)

for _ in range(100):
    out = []
    s = 0
    for i in range(n):
        s = (s + lst[i]) % 10
        out.append(s)
    lst = out

print("".join(map(str, lst[:-9:-1])))
