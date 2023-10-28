from lib import *

input = read_input(2021, 16)

lines = input.splitlines()


bits = io.StringIO("".join(bin(int(x, 16))[2:].zfill(4) for x in lines[0]))


def read(n):
    return int(bits.read(n), 2)


def parse1():
    version = read(3)
    t = read(3)
    if t == 4:
        lit = 0
        while True:
            c = read(1)
            lit *= 16
            lit += read(4)
            if not c:
                break
        return version
    else:
        vals = []
        if not read(1):
            c = read(15) + bits.tell()
            while bits.tell() < c:
                vals.append(parse1())
        else:
            for _ in range(read(11)):
                vals.append(parse1())
        return version + sum(vals)


print(parse1())


bits = io.StringIO("".join(bin(int(x, 16))[2:].zfill(4) for x in lines[0]))


def parse2():
    read(3)
    t = read(3)
    if t == 4:
        lit = 0
        while True:
            c = read(1)
            lit *= 16
            lit += read(4)
            if not c:
                break
        return lit
    else:
        vals = []
        if not read(1):
            c = read(15) + bits.tell()
            while bits.tell() < c:
                vals.append(parse2())
        else:
            for _ in range(read(11)):
                vals.append(parse2())
        match t:
            case 0:
                return sum(vals)
            case 1:
                return product(vals)
            case 2:
                return min(vals)
            case 3:
                return max(vals)
            case 5:
                return int(vals[0] > vals[1])
            case 6:
                return int(vals[0] < vals[1])
            case 7:
                return int(vals[0] == vals[1])


print(parse2())
