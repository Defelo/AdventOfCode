*lst, = map(int, open("input.txt").read().strip())
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
