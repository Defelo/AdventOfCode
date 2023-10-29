from lib import *

input = read_input(2015, 22)

(*_, boss_hp), (*_, boss_damage) = map(str.split, input.splitlines())

boss_hp, boss_damage = map(int, [boss_hp, boss_damage])


player_hp = 50
player_mana = 500


def tick(*timers):
    return [max(0, t - 1) for t in timers]


def fight_boss(player, boss, mana, shield_timer, poison_timer, recharge_timer):
    armor = 7 if shield_timer else 0

    if poison_timer:
        boss -= 3

    if recharge_timer:
        mana += 101

    shield_timer, poison_timer, recharge_timer = tick(shield_timer, poison_timer, recharge_timer)

    if boss <= 0:
        return 0

    return fight_player(player - max(1, boss_damage - armor), boss, mana, shield_timer, poison_timer, recharge_timer)


def fight_player(player, boss, mana, shield_timer, poison_timer, recharge_timer):
    if player <= 0:
        return None

    if poison_timer:
        boss -= 3

    if recharge_timer:
        mana += 101

    shield_timer, poison_timer, recharge_timer = tick(shield_timer, poison_timer, recharge_timer)

    if boss <= 0:
        return 0

    out = []

    # Magic Missile costs 53 mana. It instantly does 4 damage.

    if (
        mana >= 53
        and (x := fight_boss(player, boss - 4, mana - 53, shield_timer, poison_timer, recharge_timer)) is not None
    ):
        out.append(x + 53)

    # Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.

    if (
        mana >= 73
        and (x := fight_boss(player + 2, boss - 2, mana - 73, shield_timer, poison_timer, recharge_timer)) is not None
    ):
        out.append(x + 73)

    # Shield costs 113 mana. It starts an effect that lasts for 6 turns.

    # While it is active, your armor is increased by 7.

    if (
        mana >= 113
        and not shield_timer
        and (x := fight_boss(player, boss, mana - 113, 6, poison_timer, recharge_timer)) is not None
    ):
        out.append(x + 113)

    # Poison costs 173 mana. It starts an effect that lasts for 6 turns.

    # At the start of each turn while it is active, it deals the boss 3 damage.

    if (
        mana >= 173
        and not poison_timer
        and (x := fight_boss(player, boss, mana - 173, shield_timer, 6, recharge_timer)) is not None
    ):
        out.append(x + 173)

    # Recharge costs 229 mana. It starts an effect that lasts for 5 turns.

    # At the start of each turn while it is active, it gives you 101 new mana.

    if (
        mana >= 229
        and not recharge_timer
        and (x := fight_boss(player, boss, mana - 229, shield_timer, poison_timer, 5)) is not None
    ):
        out.append(x + 229)

    return min(out, default=None)


print(fight_player(player_hp, boss_hp, player_mana, 0, 0, 0))


def fight_player(player, boss, mana, shield_timer, poison_timer, recharge_timer):
    player -= 1

    if player <= 0:
        return None

    if poison_timer:
        boss -= 3

    if recharge_timer:
        mana += 101

    shield_timer, poison_timer, recharge_timer = tick(shield_timer, poison_timer, recharge_timer)

    if boss <= 0:
        return 0

    out = []

    # Magic Missile costs 53 mana. It instantly does 4 damage.

    if (
        mana >= 53
        and (x := fight_boss(player, boss - 4, mana - 53, shield_timer, poison_timer, recharge_timer)) is not None
    ):
        out.append(x + 53)

    # Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.

    if (
        mana >= 73
        and (x := fight_boss(player + 2, boss - 2, mana - 73, shield_timer, poison_timer, recharge_timer)) is not None
    ):
        out.append(x + 73)

    # Shield costs 113 mana. It starts an effect that lasts for 6 turns.

    # While it is active, your armor is increased by 7.

    if (
        mana >= 113
        and not shield_timer
        and (x := fight_boss(player, boss, mana - 113, 6, poison_timer, recharge_timer)) is not None
    ):
        out.append(x + 113)

    # Poison costs 173 mana. It starts an effect that lasts for 6 turns.

    # At the start of each turn while it is active, it deals the boss 3 damage.

    if (
        mana >= 173
        and not poison_timer
        and (x := fight_boss(player, boss, mana - 173, shield_timer, 6, recharge_timer)) is not None
    ):
        out.append(x + 173)

    # Recharge costs 229 mana. It starts an effect that lasts for 5 turns.

    # At the start of each turn while it is active, it gives you 101 new mana.

    if (
        mana >= 229
        and not recharge_timer
        and (x := fight_boss(player, boss, mana - 229, shield_timer, poison_timer, 5)) is not None
    ):
        out.append(x + 229)

    return min(out, default=None)


print(fight_player(player_hp, boss_hp, player_mana, 0, 0, 0))
