import re

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

    def output(self):
        out = "".join(map(chr, self.out))
        self.out.clear()
        return out

    def input(self, inp):
        self.inp += map(ord, inp + "\n")
        self.cont()

class Bot(IntCode):
    def __init__(self, mem):
        super().__init__(mem)

        self.directions = []
        self.items = []
        self.room = None

    def start(self):
        super().start()

        self.parse_new_room()

    def parse_new_room(self):
        out = self.output().split("\n")
        for line in out:
            match = re.match(r"^== (.+) ==$", line)
            if match:
                self.room = match.group(1)
                break
        out = out[out.index("Doors here lead:")+1:]
        self.directions = [d[2:] for d in out[:out.index("")]]
        if "Items here:" not in out:
            self.items.clear()
            return
        out = out[out.index("Items here:")+1:]
        self.items = [i[2:] for i in out[:out.index("")]]

    def move(self, direction):
        self.input(direction)
        self.parse_new_room()

    def take(self, item):
        self.input("take " + item)
        self.out.clear()

    def drop(self, item):
        self.input("drop " + item)
        self.out.clear()

    def inv(self):
        self.input("inv")
        out = self.output().split("\n")
        if "Items in your inventory:" not in out:
            return []
        out = out[out.index("Items in your inventory:")+1:]
        return [i[2:] for i in out[:out.index("")]]
    
    def test(self, direction):
        assert self.room == "Security Checkpoint"
        self.input(direction)

        orig = out = self.output().split("\n")
        out = out[out.index("Doors here lead:")+1:]
        out = out[out.index("")+1:]
        res = "ejected back" not in out[0]
        if res:
            print(re.match(r"^.*?(\d+).*$", out[2]).group(1))
        return res

BACK = {"north": "south", "south": "north", "east": "west", "west": "east"}

*mem, = map(int, open("input.txt").read().split(","))
bot = Bot(mem)
bot.start()

items = []
maze = {}
def explore(r=None, came_from=None, indent=0):
    room = bot.room
    if r:
        maze.setdefault(r, {})[BACK[came_from]] = room
    if room == "Security Checkpoint":
        for direction in bot.directions:
            if direction != came_from:
                maze.setdefault(room, {})[direction] = "TEST"
        return
    for item in bot.items:
        if item not in {"escape pod", "giant electromagnet", "molten lava", "photons", "infinite loop"}:
            items.append(item)
            bot.take(item)

    for direction in bot.directions:
        if came_from == direction:
            continue
        bot.move(direction)
        explore(room, BACK[direction], indent + 1)
        bot.move(BACK[direction])

explore()

def make_path(room, dest, f=None):
    if room == dest:
        return []

    for d, r in maze.get(room, {}).items():
        if r == f:
            continue
        res = make_path(r, dest, room)
        if res is not None:
            return [d] + res

for d in make_path(bot.room, "Security Checkpoint"):
    bot.move(d)
dire, = maze[bot.room]

def test_combination(i):
    for e in bot.inv():
        bot.drop(e)
    for j, e in enumerate(items):
        if i & (1 << j):
            bot.take(e)
    return bot.test(dire)

for i in range(1<<len(items)):
    if test_combination(i):
        break
