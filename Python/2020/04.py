from lib import *

input = read_input(2020, 4)


out = 0
for kv in map(str.split, input.split("\n\n")):
    x = {x.split(":")[0] for x in kv}
    out += all(e in x for e in ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])

print(out)


def valid(x):
    if not re.match(r"^\d{4}$", x.get("byr", "")) or not 1920 <= int(x["byr"]) <= 2002:
        return False

    if not re.match(r"^\d{4}$", x.get("iyr", "")) or not 2010 <= int(x["iyr"]) <= 2020:
        return False

    if not re.match(r"^\d{4}$", x.get("eyr", "")) or not 2020 <= int(x["eyr"]) <= 2030:
        return False

    if m := re.match(r"^(\d+)(in|cm)$", x.get("hgt", "")):
        a, b = m.groups()

        if b == "cm" and not 150 <= int(a) <= 193:
            return False

        elif b == "in" and not 59 <= int(a) <= 76:
            return False

    else:
        return False

    if not re.match(r"^#[\da-f]{6}$", x.get("hcl", "")):
        return False

    if x.get("ecl", "") not in "amb blu brn gry grn hzl oth".split():
        return False

    return bool(re.match(r"^\d{9}$", x.get("pid", "")))


out = 0
for kv in map(str.split, input.split("\n\n")):
    out += valid(dict(x.split(":") for x in kv))

print(out)
