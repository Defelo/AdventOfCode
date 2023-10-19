import itertools

def intcode(mem, inp):
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
            mem[mem[pc+1]] = inp.pop(0)
            pc += 2
        elif opcode == 4:
            arg1 = mem[pc+1] if mode1 else mem[mem[pc+1]]
            return arg1
            pc += 2
        elif opcode == 5:
            arg1 = mem[pc+1] if mode1 else mem[mem[pc+1]]
            arg2 = mem[pc+2] if mode2 else mem[mem[pc+2]]
            if arg1:
                pc = arg2
            else:
                pc += 3
        elif opcode == 6:
            arg1 = mem[pc+1] if mode1 else mem[mem[pc+1]]
            arg2 = mem[pc+2] if mode2 else mem[mem[pc+2]]
            if not arg1:
                pc = arg2
            else:
                pc += 3
        elif opcode == 7:
            arg1 = mem[pc+1] if mode1 else mem[mem[pc+1]]
            arg2 = mem[pc+2] if mode2 else mem[mem[pc+2]]
            mem[mem[pc+3]] = int(arg1 < arg2)
            pc += 4
        elif opcode == 8:
            arg1 = mem[pc+1] if mode1 else mem[mem[pc+1]]
            arg2 = mem[pc+2] if mode2 else mem[mem[pc+2]]
            mem[mem[pc+3]] = int(arg1 == arg2)
            pc += 4
        elif opcode == 99:
            break

*mem, = map(int, open("input.txt").read().split(","))
def test(seq):
    out = 0
    for i in seq:
        out = intcode(mem[:], [i, out])
    return out

print(max(test(seq) for seq in itertools.permutations(range(5))))
