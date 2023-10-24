def ceil(a, b):
    return a // b + (a % b > 0)

reactions = {}
for line in open("input.txt").read().splitlines():
    reactants, product = line.split(" => ")
    reactants = [reactant.split() for reactant in reactants.split(", ")]
    amount, product = product.split()
    assert product not in reactions
    reactions[product] = ([(reactant, int(amount)) for amount, reactant in reactants], int(amount))

def get_ore_used(amount):
    ore_used = 0
    chemicals = {}
    Q = [("FUEL", amount)]
    while Q:
        chemical, prod_amount = Q.pop(0)
        if chemical == "ORE":
            ore_used += prod_amount
            continue
        chem_amount = chemicals.get(chemical, 0)
        reaction_amount = prod_amount - chem_amount
        if reaction_amount <= 0:
            chemicals[chemical] -= prod_amount
            continue
        reactants, product_amount = reactions[chemical]
        reaction_count = ceil(reaction_amount, product_amount)
        for reactant, amount in reactants:
            Q.append((reactant, amount * reaction_count))
        chemicals[chemical] = chem_amount + reaction_count * product_amount - prod_amount
    return ore_used

LIMIT = 1000000000000
low = 0
high = LIMIT
while low < high:
    guess = (low + high) // 2
    used = get_ore_used(guess)
    if used <= LIMIT:
        if get_ore_used(guess + 1) > LIMIT:
            print(guess)
            break
        low = guess
    elif used > LIMIT:
        high = guess
