from lib import *

input = read_input(2019, 11)


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
o = {}
x = y = 0
dx = 0
dy = -1
while intcode.running:
    intcode.inp.append(o.get((x, y), 0))
    intcode.cont()
    o[(x, y)] = intcode.out.pop(0)
    if intcode.out.pop(0):
        dx, dy = -dy, dx
    else:
        dx, dy = dy, -dx
    x += dx
    y += dy
print(len(o))


(*mem,) = map(int, input.split(","))
intcode = IntCode(mem)
intcode.start()
o = {(0, 0): 1}
x = y = 0
dx = 0
dy = -1
minx = miny = maxx = maxy = 0
while intcode.running:
    intcode.inp.append(o.get((x, y), 0))
    intcode.cont()
    o[(x, y)] = intcode.out.pop(0)
    minx = min(x, minx)
    miny = min(y, miny)
    maxx = max(x, maxx)
    maxy = max(y, maxy)
    if intcode.out.pop(0):
        dx, dy = -dy, dx
    else:
        dx, dy = dy, -dx
    x += dx
    y += dy
for y in range(miny, maxy + 1):
    print("".join(2 * " #"[o.get((x, y), 0)] for x in range(minx, maxx + 1)))
