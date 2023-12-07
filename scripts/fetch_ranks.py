#!/usr/bin/env python

import re

import requests
from bs4 import BeautifulSoup, NavigableString

s = requests.session()

with open(".cache/session") as f:
    session = s.cookies["session"] = f.read()

me = re.search(r"<code>((\d+)-[\da-f]+)</code>", s.get("https://adventofcode.com/leaderboard/private", cookies={"session": session}).text)[1].split("-")[0]  # type: ignore

file = open(".leaderboard.csv", "w")

for year in range(2015, 2024):
    scores = {}
    names = {}

    for day in range(1, 26):
        print(f"\r{year}/{day:02} ", end="")
        resp = s.get(f"https://adventofcode.com/{year}/leaderboard/day/{day}")
        if resp.status_code == 404:
            break
        resp.encoding = "utf-8"
        bs = BeautifulSoup(resp.text, "html.parser")
        for entry in bs.select(".leaderboard-entry"):
            user_id = entry["data-user-id"]
            rank = int(re.search(r"\d+", entry.select_one(".leaderboard-position").text)[0])  # type: ignore
            name = [
                str(c).strip()
                for c in entry.descendants
                if type(c) is NavigableString
                and str(c).strip()
                and not ({*c.parent.get("class", [])} & {"supporter-badge", "sponsor-badge"})  # type: ignore
            ][-1]
            score = 101 - rank
            scores[user_id] = scores.get(user_id, 0) + score
            names[user_id] = name

    leaderboard = sorted(scores, key=scores.get, reverse=True)  # type: ignore
    ranks = {}
    last = None
    for i, user_id in enumerate(leaderboard):
        ranks[user_id] = i + 1 if last is None or scores[user_id] != scores[last] else ranks[last]
        last = user_id
        # print(ranks[user_id], scores[user_id], names[user_id])

    print(f"\r{year}: score={scores.get(me, 0)}, rank={ranks.get(me)}, total={len(ranks)}")
    file.write(",".join(map(str, [year, scores.get(me, 0), ranks.get(me), len(ranks)])) + "\n")

file.flush()
file.close()
