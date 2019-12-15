import heapq

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
        mode = self.mem[self.pc] // (10 ** (i+1)) % 10
        out = self.mem[self.pc + i]
        if mode == 1:
            return out
        elif mode == 2:
            out += self.rel
        return self.mem.get(out, 0)
    
    def write_arg(self, i, value):
        mode = self.mem[self.pc] // (10 ** (i+1)) % 10
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

NORTH, SOUTH, WEST, EAST = range(1, 5)
DIRECTIONS = [(0, -1), (0, 1), (-1, 0), (1, 0)]
BACK = {NORTH: SOUTH, SOUTH: NORTH, WEST: EAST, EAST: WEST}
HIT_WALL, MOVED, REACHED_GOAL = range(3)
UNKNOWN, EMPTY, WALL, GOAL = range(4)
grid = {(0, 0): EMPTY}

def get(x, y):
    return grid.get((x, y), 0)

def put(x, y, v):
    grid[(x, y)] = v

def explore(x, y):
    for direction, (dx, dy) in enumerate(DIRECTIONS):
        if get(x+dx, y+dy):
            continue

        intcode.inp.append(direction + 1)
        intcode.cont()
        result = intcode.out.pop(0)
        if result == HIT_WALL:
            put(x+dx, y+dy, WALL)
            continue
        elif result == MOVED:
            put(x+dx, y+dy, EMPTY)
        elif result == REACHED_GOAL:
            put(x+dx, y+dy, GOAL)
        explore(x+dx, y+dy)
        intcode.inp.append(BACK[direction + 1])
        intcode.cont()
        intcode.out.pop(0)

*mem, = map(int, open("input.txt").read().split(","))
intcode = IntCode(mem)
intcode.start()
explore(0, 0)

Q = [(0, 0, 0)]
visited = set()
while Q:
    d, x, y = heapq.heappop(Q)
    
    if (x, y) in visited:
        continue
    visited.add((x, y))

    if get(x, y) == GOAL:
        print(d)
        break

    for dx, dy in DIRECTIONS:
        if get(x+dx, y+dy) != WALL:
            heapq.heappush(Q, (d + 1, x+dx, y+dy))
