from lib import *

input = read_input(2021, 4)


nums, *boards = input.split("\n\n")
nums = ints(nums)
boards = [[[*map(int, line.split())] for line in b.splitlines()] for b in boards]
marked = set()
for num in nums:
    marked.add(num)

    for b in boards:
        if any(all(x in marked for x in row) for row in b) or any(all(x in marked for x in col) for col in zip(*b)):
            score = num * sum(x for row in b for x in row if x not in marked)
            print(score)
            break
    else:
        continue
    break


won = set()
marked = set()
for num in nums:
    marked.add(num)

    for i, b in enumerate(boards):
        if i in won:
            continue

        if any(all(x in marked for x in row) for row in b) or any(all(x in marked for x in col) for col in zip(*b)):
            won.add(i)

            if len(won) == len(boards):
                score = num * sum(x for row in b for x in row if x not in marked)

                print(score)
                break
    else:
        continue
    break
