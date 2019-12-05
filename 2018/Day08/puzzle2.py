*nums, = map(int, open("input.txt").read().split())

def get_value():
    child_count = nums.pop(0)
    meta_count = nums.pop(0)
    out = 0

    childs = [get_value() for _ in range(child_count)]

    for _ in range(meta_count):
        num = nums.pop(0)
        if not child_count:
            out += num
        elif 1 <= num <= child_count:
            out += childs[num - 1]
    return out

print(get_value())
