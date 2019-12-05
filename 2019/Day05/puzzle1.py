*mem, = map(int, open("input.txt").read().split(","))
pc = 0
while pc < len(mem):
    opcode = mem[pc] % 100
    mode1 = mem[pc] // 100 % 10
    mode2 = mem[pc] // 1000 % 10

    if opcode == 1:
        arg1 = mem[pc+1] if mode1 else mem[mem[pc+1]]
        arg2 = mem[pc+2] if mode2 else mem[mem[pc+2]]
        mem[mem[pc+3]] = arg1 + arg2
        pc += 4
    elif opcode == 2:
        arg1 = mem[pc+1] if mode1 else mem[mem[pc+1]]
        arg2 = mem[pc+2] if mode2 else mem[mem[pc+2]]
        mem[mem[pc+3]] = arg1 * arg2
        pc += 4
    elif opcode == 3:
        mem[mem[pc+1]] = 1
        pc += 2
    elif opcode == 4:
        arg1 = mem[pc+1] if mode1 else mem[mem[pc+1]]
        print(arg1)
        pc += 2
    elif opcode == 99:
        break
