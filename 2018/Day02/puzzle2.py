def compare(a, b):
    return len(a) == len(b) and sum(x != y for x, y in zip(a, b)) == 1

lines = open("input.txt").read().splitlines()
for a in lines:
    for b in lines:
        if compare(a, b):
            print("".join(x for x, y in zip(a, b) if x == y))
            exit()
