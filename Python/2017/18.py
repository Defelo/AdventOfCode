from lib import *

input = read_input(2017, 18)

lines = input.splitlines()


registers = {}
lp = None
pc = 0
while pc in range(len(lines)):
    cmd, *args = lines[pc].split()
    get = lambda a: int(a) if a.removeprefix("-").isnumeric() else registers.setdefault(a, 0)
    if cmd == "snd":
        lp = get(args[0])
    elif cmd == "set":
        registers[args[0]] = get(args[1])
    elif cmd == "add":
        registers[args[0]] = get(args[0]) + get(args[1])
    elif cmd == "mul":
        registers[args[0]] = get(args[0]) * get(args[1])
    elif cmd == "mod":
        registers[args[0]] = get(args[0]) % get(args[1])
    elif cmd == "rcv":
        if get(args[0]):
            print(lp)
            break
    elif cmd == "jgz":
        if get(args[0]) > 0:
            pc += get(args[1])
            continue
    pc += 1


class VM:
    def __init__(self, p):
        self.registers = {"p": p}
        self.pc = 0
        self.inp = []
        self.msg = None
        self.cnt = 0

    def step(self):
        if self.pc not in range(len(lines)):
            return False
        cmd, *args = lines[self.pc].split()
        get = lambda a: int(a) if a.removeprefix("-").isnumeric() else self.registers.setdefault(a, 0)
        if cmd == "snd":
            self.cnt += 1
            self.msg(get(args[0]))
        elif cmd == "set":
            self.registers[args[0]] = get(args[1])
        elif cmd == "add":
            self.registers[args[0]] = get(args[0]) + get(args[1])
        elif cmd == "mul":
            self.registers[args[0]] = get(args[0]) * get(args[1])
        elif cmd == "mod":
            self.registers[args[0]] = get(args[0]) % get(args[1])
        elif cmd == "rcv":
            if not self.inp:
                return False
            self.registers[args[0]] = self.inp.pop(0)
        elif cmd == "jgz":
            if get(args[0]) > 0:
                self.pc += get(args[1])
                return True
        self.pc += 1
        return True


p0 = VM(0)
p1 = VM(1)
p0.msg = p1.inp.append
p1.msg = p0.inp.append
while p0.step() + p1.step():
    pass

print(p1.cnt)
