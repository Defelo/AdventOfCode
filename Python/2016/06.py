from lib import *

input = read_input(2016, 6)

print("".join([Counter(x).most_common(1)[0][0] for x in zip(*input.splitlines())]))
print("".join([Counter(x).most_common()[-1][0] for x in zip(*input.splitlines())]))
