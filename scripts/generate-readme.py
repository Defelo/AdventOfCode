import base64
from collections import Counter
from datetime import date
from pathlib import Path

names = {"rs": "Rust", "hs": "Haskell", "py": "Python", "apl": "APL", "ua": "Uiua"}
exts = {"rs": [".rs"], "hs": [".hs"], "py": [".py"], "apl": [".apl"], "ua": [".ua"]}

logos = {k.name.split(".")[0]: str(k) for k in Path(".assets").iterdir()}


def logo(lang, height=12):
    return f'<img height={height} src="{logos[lang]}">'


def link(year, day, lang):
    for ext in exts[lang]:
        p = Path(f"{names[lang]}/{year}/{day:02}{ext}")
        if not p.exists():
            continue

        if lang == "ua":
            src = '&fwa "../lib.ua" "Load ← &sc"\n'.encode() + p.read_bytes()
            url = f"https://uiua.org/pad?src={base64.urlsafe_b64encode(src).decode()}"
        else:
            url = f"{names[lang]}/{year}/{day:02}{ext}"

        return f' [{logo(lang)}]({url} "{names[lang]} solution for {year}/{day:02}")'

    return ""


print("# AdventOfCode")

lst = [f"[{logo(k)} {v}]({v})" for k, v in names.items()]
print(f"[Advent of Code](https://adventofcode.com/) solutions in {', '.join(lst[:-1])} and {lst[-1]}")

print()
print("### Global Leaderboard Placement")
print("|Year|Rank|Score|")
print("|-|-|-|")
with open(".leaderboard.csv") as f:
    for line in reversed(f.readlines()):
        year, score, rank, _ = line.strip().split(",")
        if rank != "None":
            print(f"|[{year}](https://adventofcode.com/{year}/leaderboard)|**{rank}**|{score}|")

for year in range(2023, 2014, -1):
    lines = []
    line = [""] * date(year, 12, 1).weekday()
    langs = Counter()
    for day in range(1, 26):
        if len(line) == 7:
            lines.append(line)
            line = []
        line.append(f"[**{day}**](https://adventofcode.com/{year}/day/{day})")
        for lang in names:
            l = link(year, day, lang)
            if l:
                langs.update([lang])
                line[-1] += l

    for day in range(26, 32):
        if len(line) == 7:
            lines.append(line)
            line = []
        line.append(f"{day}")

    s = [f"[{logo(k, height=18)} {names[k]}]({names[k]}/{year}): {v}/25" for k, v in langs.most_common()]
    print()
    print(f"## [{year}](https://adventofcode.com/{year})" + f" ({' | '.join(s)})" * bool(s))
    print("|Mo|Tu|We|Th|Fr|Sa|Su|")
    print("|-|-|-|-|-|-|-|")
    for line in lines + [line] * bool(line):
        print("|" + "|".join(line + [""] * (7 - len(line))) + "|")
