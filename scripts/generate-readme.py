from datetime import date
from pathlib import Path

names = {"rs": "Rust", "hs": "Haskell", "py": "Python", "apl": "APL"}
exts = {"rs": ["rs"], "hs": ["hs"], "py": ["py", "ipynb"], "apl": ["ipynb"]}
logos = {
    "rs": "https://rustacean.net/assets/rustacean-flat-noshadow.svg",
    "hs": "https://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg",
    "py": "https://upload.wikimedia.org/wikipedia/commons/c/c3/Python-logo-notext.svg",
    "apl": "https://aplwiki.com/images/thumb/c/c6/APL_logo.png/600px-APL_logo.png",
}


def link(year, day, lang):
    for ext in exts[lang]:
        if not Path(f"{names[lang]}/{year}/{day:02}.{ext}").exists():
            continue

        return f' [<img height=12 src="{logos[lang]}">]({names[lang]}/{year}/{day:02}.{ext} "{names[lang]} solution for {year}/{day:02}")'

    return ""


print("# AdventOfCode")
print(
    f"[Advent of Code](https://adventofcode.com/) solutions in {', '.join([*names.values()][:-1])} and {[*names.values()][-1]}"
)

for year in range(2022, 2014, -1):
    print()
    print(f"## [{year}](https://adventofcode.com/{year})")
    print("|Mo|Tu|We|Th|Fr|Sa|Su|")
    print("|-|-|-|-|-|-|-|")

    line = [""] * date(year, 12, 1).weekday()
    for day in range(1, 26):
        if len(line) == 7:
            print("|" + "|".join(line) + "|")
            line.clear()
        line.append(f"[**{day}**](https://adventofcode.com/{year}/day/{day})")
        for lang in names:
            line[-1] += link(year, day, lang)

    for day in range(26, 32):
        if len(line) == 7:
            print("|" + "|".join(line) + "|")
            line.clear()
        line.append(f"{day}")

    if line:
        print("|" + "|".join(line + [""] * (7 - len(line))) + "|")
