*mem, = map(int, open("input.txt").read().strip().split(","))
i = 0
mem[1] = 12
mem[2] = 2
while i < len(mem):
    if mem[i] == 99:
        break
    elif mem[i] == 1:
        mem[mem[i+3]] = mem[mem[i+1]] + mem[mem[i+2]]
    elif mem[i] == 2:
        mem[mem[i+3]] = mem[mem[i+1]] * mem[mem[i+2]]
    i += 4
print(mem[0])
