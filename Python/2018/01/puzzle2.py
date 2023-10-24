lines = open("input.txt").read().splitlines()
freq = 0
seen = {0}
while True:
    for line in lines:
        freq += int(line)
        if freq in seen:
            print(freq)
            exit()
        else:
            seen.add(freq)
