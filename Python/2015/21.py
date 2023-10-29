from lib import *

input = read_input(2015, 21)

(*_, boss_hp), (*_, boss_damage), (*_, boss_armor) = map(str.split, input.splitlines())

boss_hp, boss_damage, boss_armor = map(int, [boss_hp, boss_damage, boss_armor])


player_hp = 100


NEUTRAL = (0, 0, 0)
shop_weapons = [(8, 4, 0), (10, 5, 0), (25, 6, 0), (40, 7, 0), (74, 8, 0)]
shop_armor = [NEUTRAL, (13, 0, 1), (31, 0, 2), (53, 0, 3), (75, 0, 4), (102, 0, 5)]
shop_rings = [NEUTRAL, NEUTRAL, (25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3)]


def fight(player_damage, player_armor):
    player = player_hp
    boss = boss_hp
    while True:
        boss -= max(1, player_damage - boss_armor)
        if boss <= 0:
            return True
        player -= max(1, boss_damage - player_armor)
        if player <= 0:
            return False


def combine(*args):
    return tuple(map(sum, zip(*args)))


best = 1e1337
for weapon in shop_weapons:
    for armor in shop_armor:
        for i, ring1 in enumerate(shop_rings):
            for ring2 in shop_rings[i + 1 :]:
                cost, d, a = combine(weapon, armor, ring1, ring2)
                if fight(d, a):
                    best = min(best, cost)
print(best)


best = 0
for weapon in shop_weapons:
    for armor in shop_armor:
        for i, ring1 in enumerate(shop_rings):
            for ring2 in shop_rings[i + 1 :]:
                cost, d, a = combine(weapon, armor, ring1, ring2)
                if not fight(d, a):
                    best = max(best, cost)
print(best)
