from lib import *

input = read_input(2020, 19)


raw_rules, messages = map(str.splitlines, input.split("\n\n"))
raw_rules = dict(line.split(": ") for line in raw_rules)
rules = {}


def make_rule1(idx):
    if idx in rules:
        return rules[idx]

    out = []
    for sub_rules in raw_rules[idx].split(" | "):
        o = ""
        for sr in sub_rules.split():
            if x := re.match(r'"(.+)"', sr):
                o += x[1]
            else:
                o += make_rule1(sr)
        out.append(o)
    rules[idx] = "(" + "|".join(out) + ")"
    return rules[idx]


pattern = re.compile(f"^{make_rule1('0')}$")

print(sum(bool(pattern.match(msg)) for msg in messages))


raw_rules, messages = map(str.splitlines, input.split("\n\n"))
raw_rules = dict(line.split(": ") for line in raw_rules)

rules = {}


def make_rule2(idx):
    if idx in rules:
        return rules[idx]

    out = []
    if idx == "8":
        out.append(make_rule2("42") + "+")
    elif idx == "11":
        for k in range(1, 10):
            out.append(make_rule2("42") * k + make_rule2("31") * k)
    else:
        for sub_rules in raw_rules[idx].split(" | "):
            o = ""
            for sr in sub_rules.split():
                if x := re.match(r'"(.+)"', sr):
                    o += x[1]
                else:
                    o += make_rule2(sr)
            out.append(o)
    rules[idx] = "(" + "|".join(out) + ")"
    return rules[idx]


pattern = re.compile(f"^{make_rule2('0')}$")

print(sum(bool(pattern.match(msg)) for msg in messages))
