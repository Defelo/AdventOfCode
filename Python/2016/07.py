from lib import *

input = read_input(2016, 7)


def check(ip):
    tmp = ""
    c = 0
    ok = False
    for x in ip:
        tmp += x
        if x == "[":
            c += 1
            tmp = ""
        elif x == "]":
            c -= 1
            tmp = ""
        elif len(tmp) >= 4:
            if tmp[-1] == tmp[-4] and tmp[-2] == tmp[-3] and tmp[-1] != tmp[-2]:
                if c:
                    return False
                ok = True
    return ok


print(sum(check(line) for line in input.splitlines()))


def check(ip):
    supernet = []
    hypernet = []
    tmp = ""
    c = 0
    for x in ip:
        k = tmp
        tmp += x
        if x == "[":
            if k:
                [hypernet, supernet][not c].append(k)
            c += 1
            tmp = ""
        elif x == "]":
            if k:
                [hypernet, supernet][not c].append(k)
            c -= 1
            tmp = ""
    if tmp:
        [hypernet, supernet][not c].append(tmp)

    for sup in supernet:
        for i in range(len(sup) - 2):
            if sup[i] != sup[i + 2] or sup[i] == sup[i + 1]:
                continue
            if any(sup[i + 1] + sup[i] + sup[i + 1] in hyp for hyp in hypernet):
                return True
    return False


print(sum(check(line) for line in input.splitlines()))
