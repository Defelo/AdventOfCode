a, b = map(int, open("input.txt").read().split("-"))
count = 0
for i in range(a, b + 1):
    string = str(i)
    if list(string) != sorted(string):
        continue
    if any(x==y for x, y in zip(string[1:], string)):
        count += 1
print(count)
