from lib import *

input = read_input(2019, 13)


class IntCode:
    def __init__(self, mem):
        self.mem = {i: e for i, e in enumerate(mem)}
        self.pc = 0
        self.running = False
        self.inp = []
        self.out = []
        self.rel = 0

    def start(self):
        self.running = True
        self.cont()

    def get_arg(self, i):
        mode = self.mem[self.pc] // (10 ** (i + 1)) % 10
        out = self.mem[self.pc + i]
        if mode == 1:
            return out
        elif mode == 2:
            out += self.rel
        return self.mem.get(out, 0)

    def write_arg(self, i, value):
        mode = self.mem[self.pc] // (10 ** (i + 1)) % 10
        pos = self.mem[self.pc + i]
        assert mode != 1
        if mode == 2:
            pos += self.rel
        self.mem[pos] = value

    def cont(self):
        while True:
            opcode = self.mem[self.pc] % 100

            if opcode == 1:
                self.write_arg(3, self.get_arg(1) + self.get_arg(2))
                self.pc += 4
            elif opcode == 2:
                self.write_arg(3, self.get_arg(1) * self.get_arg(2))
                self.pc += 4
            elif opcode == 3:
                if not self.inp:
                    return
                self.write_arg(1, self.inp.pop(0))
                self.pc += 2
            elif opcode == 4:
                self.out.append(self.get_arg(1))
                self.pc += 2
            elif opcode == 5:
                if self.get_arg(1):
                    self.pc = self.get_arg(2)
                else:
                    self.pc += 3
            elif opcode == 6:
                if not self.get_arg(1):
                    self.pc = self.get_arg(2)
                else:
                    self.pc += 3
            elif opcode == 7:
                self.write_arg(3, int(self.get_arg(1) < self.get_arg(2)))
                self.pc += 4
            elif opcode == 8:
                self.write_arg(3, int(self.get_arg(1) == self.get_arg(2)))
                self.pc += 4
            elif opcode == 9:
                self.rel += self.get_arg(1)
                self.pc += 2
            elif opcode == 99:
                self.running = False
                return


(*mem,) = map(int, input.split(","))
intcode = IntCode(mem)
intcode.start()
blocks = 0
while intcode.out:
    _, _, t = intcode.out[:3]
    del intcode.out[:3]
    blocks += t == 2
print(blocks)


(*mem,) = map(int, input.split(","))
mem[0] = 2
intcode = IntCode(mem)
intcode.start()
ball = None
paddle = None
score = 0
while intcode.out:
    x, _, t = intcode.out[:3]
    del intcode.out[:3]
    if t == 3:
        paddle = x
    elif t == 4:
        ball = x

while intcode.running:
    if paddle < ball:
        intcode.inp.append(1)
    elif ball < paddle:
        intcode.inp.append(-1)
    else:
        intcode.inp.append(0)
    intcode.cont()

    while intcode.out:
        x, _, t = intcode.out[:3]
        del intcode.out[:3]
        if x == -1:
            score = t
        elif t == 3:
            paddle = x
        elif t == 4:
            ball = x
print(score)
