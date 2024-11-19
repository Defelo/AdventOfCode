import base64
from collections import Counter
from datetime import date
from pathlib import Path

LATEST_YEAR = 2023

names = {"rs": "Rust", "hs": "Haskell", "py": "Python", "apl": "APL", "ua": "Uiua", "nix": "Nix", "nu": "Nu"}
exts = {"rs": [".rs"], "hs": [".hs"], "py": [".py"], "apl": [".apl"], "ua": [".ua"], "nix": [".nix"], "nu": [".nu"]}

logos = {k.name.split(".")[0]: str(k) for k in Path(".assets").iterdir()}


def logo(lang, height=12):
    return f'<img height={height} src="{logos[lang]}">'


def link(year, day, lang):
    for ext in exts[lang]:
        p = Path(f"{names[lang]}/{year}/{day:02}{ext}")
        if not p.exists():
            continue

        if lang == "ua":
            src = "&sc\n".encode() + p.read_bytes().split(b"\n", 1)[1]
            if year == 2019:
                intcode = p.parent.joinpath("Intcode.ua").read_bytes()
                intcode = b"\n".join(
                    b"  " + l for line in intcode.splitlines() if (l := line.strip()) and not l.startswith(b"#")
                )
                src = src.replace(b'Intcode ~ "Intcode.ua"', "┌─╴Intcode\n".encode() + intcode + "\n└─╴".encode(), 1)
            url = f"https://uiua.org/pad?src={base64.urlsafe_b64encode(src).decode()}"
        else:
            url = f"{names[lang]}/{year}/{day:02}{ext}"

        return f' [{logo(lang)}]({url} "{names[lang]} solution for {year}/{day:02}")'

    return ""


print("# AdventOfCode")

cnt_by_lang = {
    lang: sum(bool(link(year, day, lang)) for year in range(2015, LATEST_YEAR + 1) for day in range(1, 26))
    for lang in names
}
cnt_by_lang["py"] = min(cnt_by_lang[x] for x in ["rs", "ua"]) - 1
lst = [f"[{logo(k)} {v}]({v})" for k, v in sorted(names.items(), key=lambda x: (-cnt_by_lang[x[0]], x[1]))]
print(f"[Advent of Code](https://adventofcode.com/) solutions in {', '.join(lst[:-1])} and {lst[-1]}")

print()
print("### Global Leaderboard Placement")
print("|Year|Rank|Score|Top% (Leaderboard)|Top% (All participants)|")
print("|-|-|-|-|-|")
with open(".ranks.csv") as f:
    for line in reversed(f.readlines()):
        year, score, rank, leaderboard, total = line.strip().split(",")
        if rank != "None":
            lb = int(rank) / int(leaderboard)
            t = int(rank) / int(total)
            print(
                f"|[{year}](https://adventofcode.com/{year}/leaderboard)|**{rank}**|{score}|{lb:.2%} (of {leaderboard})|{t:.4%} (of &ge;{total})|"
            )

for year in range(LATEST_YEAR, 2014, -1):
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
