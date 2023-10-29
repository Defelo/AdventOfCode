from lib import *

input = read_input(2016, 17).strip()


def get_state(path):
    digest = hashlib.md5((input + path).encode()).hexdigest()
    return [c >= "b" for c in digest[:4]]  # up, down, left, right


def find_path():
    queue = [("", 0, 0)]
    while True:
        path, x, y = queue.pop(0)

        if x == y == 3:
            return path

        up, down, left, right = get_state(path)

        if up and y > 0:
            queue.append((path + "U", x, y - 1))

        if down and y < 3:
            queue.append((path + "D", x, y + 1))

        if left and x > 0:
            queue.append((path + "L", x - 1, y))

        if right and x < 3:
            queue.append((path + "R", x + 1, y))


print(find_path())


def find_path():
    queue = [("", 0, 0)]

    longest = ""

    while queue:
        path, x, y = queue.pop(0)

        if x == y == 3:
            if len(path) > len(longest):
                longest = path

            continue

        up, down, left, right = get_state(path)

        if up and y > 0:
            queue.append((path + "U", x, y - 1))

        if down and y < 3:
            queue.append((path + "D", x, y + 1))

        if left and x > 0:
            queue.append((path + "L", x - 1, y))

        if right and x < 3:
            queue.append((path + "R", x + 1, y))

    return longest


print(len(find_path()))
