from lib import *

input = read_input(2018, 24)


@dataclass
class Group:
    army: int
    units: int
    hp: int
    ap: int
    at: str
    init: int
    weak: set[str]
    immune: set[str]

    def __hash__(self):
        return id(self)

    @staticmethod
    def parse(army, line, boost=0):
        units, hp, _, extra, ap, at, init = re.match(
            r"^(\d+) units each with (\d+) hit points( \((.*)\))? with an attack that does (\d+) (\w+) damage at initiative (\d+)$",
            line,
        ).groups()
        weak = set()
        immune = set()
        for part in extra.split("; ") if extra else []:
            t, _, *xs = part.split()
            {"weak": weak, "immune": immune}[t].update(x.strip(",") for x in xs)

        return Group(army, int(units), int(hp), int(ap) + boost, at, int(init), weak, immune)

    @property
    def ep(self):
        return self.units * self.ap

    @property
    def dead(self):
        return self.units <= 0

    def calc_damage(self, target):
        if self.at in target.immune:
            return 0
        mul = 2 if self.at in target.weak else 1
        return self.ep * mul

    def attack(self, target):
        damage = self.calc_damage(target) // target.hp
        target.units -= damage
        return damage


immune, infect = [
    [Group.parse(i, group) for group in army.splitlines()[1:]] for i, army in enumerate(input.split("\n\n"))
]

while immune and infect:
    targets = {}
    imm_att = set(immune)
    inf_att = set(infect)
    for group in sorted(immune + infect, key=lambda g: (-g.ep, -g.init)):
        attackable = [inf_att, imm_att][group.army]
        if not attackable:
            continue

        target = max(attackable, key=lambda g: (group.calc_damage(g), g.ep, g.init))
        if not group.calc_damage(target):
            continue

        attackable.remove(target)
        targets[group] = target

    for group, target in sorted(targets.items(), key=lambda a: -a[0].init):
        if group.dead:
            continue
        group.attack(target)

    immune, infect = [[g for g in x if not g.dead] for x in [immune, infect]]

print(sum(g.units for g in immune + infect))


def test(boost):
    immune, infect = [
        [Group.parse(i, group, boost if i == 0 else 0) for group in army.splitlines()[1:]]
        for i, army in enumerate(input.split("\n\n"))
    ]

    while immune and infect:
        targets = {}
        imm_att = set(immune)
        inf_att = set(infect)
        for group in sorted(immune + infect, key=lambda g: (-g.ep, -g.init)):
            attackable = [inf_att, imm_att][group.army]
            if not attackable:
                continue

            target = max(attackable, key=lambda g: (group.calc_damage(g), g.ep, g.init))
            if not group.calc_damage(target):
                continue

            attackable.remove(target)
            targets[group] = target

        ok = False
        for group, target in sorted(targets.items(), key=lambda a: -a[0].init):
            if group.dead:
                continue

            if group.attack(target):
                ok = True

        if not ok:
            break

        immune, infect = [[g for g in x if not g.dead] for x in [immune, infect]]

    if infect:
        return None

    return sum(g.units for g in immune)


boost = 0
while not (out := test(boost)):
    boost += 1
print(out)
