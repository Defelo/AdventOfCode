from functools import reduce
import operator


def product(it):
    return reduce(operator.mul, it)


def extended_gcd(a, b):
    if a == 0:
        return b, 0, 1

    gcd, x1, y1 = extended_gcd(b % a, a)

    x = y1 - (b // a) * x1
    y = x1

    return gcd, x, y


def chinese_remainder(n, a):
    s = 0
    prod = reduce(int.__mul__, n)
    for n_i, a_i in zip(n, a):
        p = prod // n_i
        s += a_i * extended_gcd(p, n_i)[1] * p
    return s % prod


def is_prime(n):
    if n < 5:
        return n in [2, 3]
    if n % 6 not in [1, 5]:
        return False
    return all(n % i for i in range(2, int(n**0.5) + 1))
