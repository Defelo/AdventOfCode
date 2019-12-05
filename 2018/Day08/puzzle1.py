*nums, = map(int, open("input.txt").read().split())

def get_metadata_sum():
    child_count = nums.pop(0)
    meta_count = nums.pop(0)
    out = 0
    for _ in range(child_count):
        out += get_metadata_sum()
    for _ in range(meta_count):
        out += nums.pop(0)
    return out

print(get_metadata_sum())
