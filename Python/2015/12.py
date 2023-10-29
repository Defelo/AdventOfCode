from lib import *

input = read_input(2015, 12)

data = json.loads(input)


def count1(obj):
    if isinstance(obj, dict):
        return sum(map(count1, obj.values()))

    elif isinstance(obj, list):
        return sum(map(count1, obj))

    elif isinstance(obj, int):
        return obj

    return 0


print(count1(data))


def count2(obj):
    if isinstance(obj, dict):
        if "red" in obj.values():
            return 0

        return sum(map(count2, obj.values()))

    elif isinstance(obj, list):
        return sum(map(count2, obj))

    elif isinstance(obj, int):
        return obj

    return 0


print(count2(data))
