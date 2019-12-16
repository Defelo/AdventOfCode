*lst, = map(int, open("input.txt").read().strip())
offset = int("".join(map(str, lst[:7])))
lst = (lst * 10_000)[:offset-1:-1]
n = len(lst)

for _ in range(100):
    out = []
    s = 0
    for i in range(n):
        s = (s + lst[i]) % 10
        out.append(s)
    lst = out

print("".join(map(str, lst[:-9:-1])))

