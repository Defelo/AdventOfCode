a, b = map(int, open("input.txt").read().split("-"))
count = 0
for i in range(a, b + 1):
    string = str(i)
    if list(string) != sorted(string):
        continue
    if any(string.count(c) == 2 for c in string):
        count += 1
print(count)
