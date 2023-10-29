from lib import *

input = read_input(2015, 7)

MOD = 1 << 16


funcs = {dest: args for *args, _, dest in map(str.split, input.splitlines())}

dp = {}


def solve(key):
    if key not in dp:
        if key.isnumeric():
            val = int(key)

        else:
            args = funcs[key]

            if len(args) == 1:
                val = solve(args[0])

            elif len(args) == 2:
                if args[0] == "NOT":
                    val = ~solve(args[1]) % MOD

            elif len(args) == 3:
                if args[1] == "OR":
                    val = solve(args[0]) | solve(args[2])

                elif args[1] == "AND":
                    val = solve(args[0]) & solve(args[2])

                elif args[1] == "LSHIFT":
                    val = solve(args[0]) << solve(args[2])

                elif args[1] == "RSHIFT":
                    val = solve(args[0]) >> solve(args[2])

        dp[key] = val % MOD

    return dp[key]


print(solve("a"))

dp.clear()
dp = {"b": solve("a")}
print(solve("a"))
