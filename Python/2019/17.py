from lib import *

input = read_input(2019, 17)


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
grid = "".join(map(chr, intcode.out)).strip().splitlines()
out = 0
for i in range(1, len(grid) - 1):
    for j in range(1, len(grid[i]) - 1):
        if grid[i][j] == grid[i][j - 1] == grid[i][j + 1] == grid[i - 1][j] == grid[i + 1][j] == "#":
            out += i * j
print(out)


def apply_direction(x, y, d):
    d %= 4
    if d == 0:
        y -= 1
    elif d == 2:
        y += 1
    elif d == 1:
        x += 1
    elif d == 3:
        x -= 1
    return x, y


def is_path(x, y):
    return y in range(len(grid)) and x in range(len(grid[y])) and grid[y][x] == "#"


(*mem,) = map(int, input.split(","))
mem[0] = 2
intcode = IntCode(mem)
intcode.start()
grid = []
x = y = None
direction = 0
for i, line in enumerate("".join(map(chr, intcode.out)).strip().splitlines()):
    if "^" in line:
        x, y = line.index("^"), i
    grid.append(line)
instructions = []
while True:
    if is_path(*apply_direction(x, y, direction - 1)):
        inst = "L"
        direction -= 1
    elif is_path(*apply_direction(x, y, direction + 1)):
        inst = "R"
        direction += 1
    else:
        break

    cnt = 0
    while True:
        nx, ny = apply_direction(x, y, direction)
        if is_path(nx, ny):
            cnt += 1
            x, y = nx, ny
        else:
            break
    if cnt:
        instructions.append(inst + "," + str(cnt))


def get_groups(start, groups):
    if start >= len(instructions):
        return groups

    for i in range(3):
        if groups[i] is not None:
            if all(groups[i][j] == instructions[j + start] for j in range(len(groups[i]))):
                return get_groups(start + len(groups[i]), groups)
        else:
            groups = groups[:]
            groups[i] = []
            for j in range(1, 5):
                groups[i].append(instructions[start + j - 1])
                result = get_groups(start + j, groups)
                if result:
                    return result


groups = get_groups(0, [None, None, None])
main = []
i = 0
while i < len(instructions):
    for j, g in enumerate(groups):
        if g == instructions[i : i + len(g)]:
            main.append("ABC"[j])
            i += len(g)
            break

intcode.inp += list(map(ord, ",".join(main) + "\n"))
for g in groups:
    intcode.inp += list(map(ord, ",".join(g) + "\n"))
intcode.inp.append(ord("n"))
intcode.inp.append(ord("\n"))
intcode.cont()
print(intcode.out[-1])
