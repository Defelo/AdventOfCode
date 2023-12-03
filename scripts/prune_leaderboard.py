#!/usr/bin/env python

import re

import requests

with open(".cache/session") as f:
    s = requests.session()
    s.cookies["session"] = f.read()

user_id = re.search(r"<code>((\d+)-[\da-f]+)</code>", s.get("https://adventofcode.com/leaderboard/private").text)[1].split("-")[0]  # type: ignore

years = {
    year: s.get(f"https://adventofcode.com/{year}/leaderboard/private/view/{user_id}.json").json()["members"]
    for year in range(2015, 2024)
}
ty = max(years)
members = {k: v["name"] for k, v in next(iter(years.values())).items()}

total_scores = {m: sum(yv[m]["local_score"] for y, yv in years.items()) for m in members}
total_stars = {m: sum(yv[m]["stars"] for y, yv in years.items()) for m in members}
weighted_total_scores = {
    m: round(sum(yv[m]["local_score"] * (0.33 ** (ty - y)) for y, yv in years.items())) for m in members
}
cnt = len(members)
for i, (m, score) in [*enumerate(sorted(weighted_total_scores.items(), key=lambda x: x[1], reverse=True))][::-1]:
    total = total_scores[m]
    local = years[ty][m]["local_score"]
    stars = years[ty][m]["stars"]
    print(
        f"#{i+1:03} {total:7} {score:7} {local:7} ({local/score if score else 0:4.0%}) {total_stars[m]:3} {stars:2} {stars/total_stars[m] if total_stars[m] else 0:4.0%}  {m:10} {members[m] or '(anonymous)':30}"
    )
    if input(f"Kick this user? ({cnt}/200 left) ") == "y":
        token = re.search(r'name="csrf_token" value="([0-9a-f]+)"', s.get(f"https://adventofcode.com/{ty}/leaderboard/private/view/{user_id}").text)[1]  # type: ignore
        r = s.post(f"https://adventofcode.com/{ty}/leaderboard/private/part/{user_id}/{m}", data={"csrf_token": token})
        if r.ok:
            cnt -= 1
        else:
            print("Failed to kick user!", r, r.text)
