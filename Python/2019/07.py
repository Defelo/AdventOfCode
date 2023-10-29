from lib import *

input = read_input(2019, 7)


def intcode(mem, inp):
    pc = 0
    while pc < len(mem):
        opcode = mem[pc] % 100
        mode1 = mem[pc] // 100 % 10
        mode2 = mem[pc] // 1000 % 10

        if opcode == 1:
            arg1 = mem[pc + 1] if mode1 else mem[mem[pc + 1]]
            arg2 = mem[pc + 2] if mode2 else mem[mem[pc + 2]]
            mem[mem[pc + 3]] = arg1 + arg2
            pc += 4
        elif opcode == 2:
            arg1 = mem[pc + 1] if mode1 else mem[mem[pc + 1]]
            arg2 = mem[pc + 2] if mode2 else mem[mem[pc + 2]]
            mem[mem[pc + 3]] = arg1 * arg2
            pc += 4
        elif opcode == 3:
            mem[mem[pc + 1]] = inp.pop(0)
            pc += 2
        elif opcode == 4:
            arg1 = mem[pc + 1] if mode1 else mem[mem[pc + 1]]
            return arg1
            pc += 2
        elif opcode == 5:
            arg1 = mem[pc + 1] if mode1 else mem[mem[pc + 1]]
            arg2 = mem[pc + 2] if mode2 else mem[mem[pc + 2]]
            if arg1:
                pc = arg2
            else:
                pc += 3
        elif opcode == 6:
            arg1 = mem[pc + 1] if mode1 else mem[mem[pc + 1]]
            arg2 = mem[pc + 2] if mode2 else mem[mem[pc + 2]]
            if not arg1:
                pc = arg2
            else:
                pc += 3
        elif opcode == 7:
            arg1 = mem[pc + 1] if mode1 else mem[mem[pc + 1]]
            arg2 = mem[pc + 2] if mode2 else mem[mem[pc + 2]]
            mem[mem[pc + 3]] = int(arg1 < arg2)
            pc += 4
        elif opcode == 8:
            arg1 = mem[pc + 1] if mode1 else mem[mem[pc + 1]]
            arg2 = mem[pc + 2] if mode2 else mem[mem[pc + 2]]
            mem[mem[pc + 3]] = int(arg1 == arg2)
            pc += 4
        elif opcode == 99:
            break


(*mem,) = map(int, input.split(","))


def test(seq):
    out = 0
    for i in seq:
        out = intcode(mem[:], [i, out])
    return out


print(max(test(seq) for seq in itertools.permutations(range(5))))


class IntCode:
    def __init__(self, mem):
        self.mem = mem
        self.pc = 0
        self.running = False
        self.inp = []
        self.out = []

    def start(self):
        self.running = True
        self.cont()

    def cont(self):
        while self.pc < len(self.mem):
            opcode = self.mem[self.pc] % 100
            mode1 = self.mem[self.pc] // 100 % 10
            mode2 = self.mem[self.pc] // 1000 % 10

            if opcode == 1:
                arg1 = self.mem[self.pc + 1] if mode1 else self.mem[self.mem[self.pc + 1]]
                arg2 = self.mem[self.pc + 2] if mode2 else self.mem[self.mem[self.pc + 2]]
                self.mem[self.mem[self.pc + 3]] = arg1 + arg2
                self.pc += 4
            elif opcode == 2:
                arg1 = self.mem[self.pc + 1] if mode1 else self.mem[self.mem[self.pc + 1]]
                arg2 = self.mem[self.pc + 2] if mode2 else self.mem[self.mem[self.pc + 2]]
                self.mem[self.mem[self.pc + 3]] = arg1 * arg2
                self.pc += 4
            elif opcode == 3:
                if not self.inp:
                    return
                self.mem[self.mem[self.pc + 1]] = self.inp.pop(0)
                self.pc += 2
            elif opcode == 4:
                arg1 = self.mem[self.pc + 1] if mode1 else self.mem[self.mem[self.pc + 1]]
                self.out.append(arg1)
                self.pc += 2
            elif opcode == 5:
                arg1 = self.mem[self.pc + 1] if mode1 else self.mem[self.mem[self.pc + 1]]
                arg2 = self.mem[self.pc + 2] if mode2 else self.mem[self.mem[self.pc + 2]]
                if arg1:
                    self.pc = arg2
                else:
                    self.pc += 3
            elif opcode == 6:
                arg1 = self.mem[self.pc + 1] if mode1 else self.mem[self.mem[self.pc + 1]]
                arg2 = self.mem[self.pc + 2] if mode2 else self.mem[self.mem[self.pc + 2]]
                if not arg1:
                    self.pc = arg2
                else:
                    self.pc += 3
            elif opcode == 7:
                arg1 = self.mem[self.pc + 1] if mode1 else self.mem[self.mem[self.pc + 1]]
                arg2 = self.mem[self.pc + 2] if mode2 else self.mem[self.mem[self.pc + 2]]
                self.mem[self.mem[self.pc + 3]] = int(arg1 < arg2)
                self.pc += 4
            elif opcode == 8:
                arg1 = self.mem[self.pc + 1] if mode1 else self.mem[self.mem[self.pc + 1]]
                arg2 = self.mem[self.pc + 2] if mode2 else self.mem[self.mem[self.pc + 2]]
                self.mem[self.mem[self.pc + 3]] = int(arg1 == arg2)
                self.pc += 4
            elif opcode == 99:
                self.running = False
                return


(*mem,) = map(int, input.split(","))


def test(seq):
    out = 0
    instances = []
    for i in range(5):
        instances.append(IntCode(mem[:]))
        instances[-1].inp.append(seq[i])
        instances[-1].start()

    i = 0
    while instances[-1].running:
        instances[i].inp.append(out)
        instances[i].cont()
        out = instances[i].out.pop()
        i = (i + 1) % 5
    return out


print(max(test(seq) for seq in itertools.permutations(range(5, 10))))
