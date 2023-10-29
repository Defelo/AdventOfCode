from lib import *

input = read_input(2015, 5)


def is_nice1(x):
    return bool(re.match(r"^(?=(.*[aeiou]){3,})(?=.*(?P<x>.)(?P=x)).*$", x)) and not any(
        e in x for e in ["ab", "cd", "pq", "xy"]
    )


print(sum(map(is_nice1, input.splitlines())))


def is_nice2(x):
    return bool(re.match(r"^(?=.*(?P<a>..).*(?P=a))(?=.*(?P<b>.).(?P=b)).*$", x))


print(sum(map(is_nice2, input.splitlines())))
