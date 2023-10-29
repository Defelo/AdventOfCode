from lib import *

input = read_input(2016, 4)


out = 0

for room in input.splitlines():
    name, sector, checksum = re.match(r"^([a-z\-]+)-(\d+)\[([a-z]+)\]$", room).groups()

    if [*checksum] == [a[0] for a in sorted(Counter(name.replace("-", "")).items(), key=lambda a: (-a[1], a[0]))[:5]]:
        out += int(sector)

print(out)
for room in input.splitlines():
    name, sector = re.match(r"^([a-z\-]+)-(\d+)\[[a-z]+\]$", room).groups()

    name = "".join(chr((ord(c) - 0x61 + int(sector)) % 26 + 0x61) if c != "-" else " " for c in name)

    if "north" in name:
        print(sector)
        break
