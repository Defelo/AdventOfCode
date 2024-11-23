#!/usr/bin/env bash

set -e

prog=$(realpath "$0")
while ! [[ -e flake.nix ]]; do cd ..; done

year=$(date +%Y)
month=$(date +%m)
day=$(date +%d)
day=${day#0}

usage() {
  echo "usage: aoc-download-input [YEAR DAY] [--live]"
  echo "       aoc-download-input --all"
  exit 1
}

if [[ $# -eq 1 ]] && [[ "$1" = "--all" ]]; then
  for y in $(seq 2015 $((year-1))); do
    for d in {1..25}; do
      bash "$prog" $y $d
    done
  done
  if [[ $month -eq 12 ]]; then
    y=$year
    for d in $(seq 1 $(( $day > 25 ? 25 : $day ))); do
      bash "$prog" $y $d
    done
  fi
  exit
elif [[ $# -eq 1 ]] && [[ "$1" = "--live" ]]; then
  TZ="Etc/GMT+4" date +"%Y %m %d" | read year month day
  day=${day#0}
  if [[ $month -ne 12 ]] || [[ $day -gt 25 ]]; then
    usage
  fi
elif [[ $# -eq 2 ]] || [[ $# -eq 3 ]]; then
  year=$1
  day=$2
elif [[ $# -ne 0 ]] || [[ $month -ne 12 ]] || [[ $day -gt 25 ]]; then
  usage
fi

echo $year/$day

mkdir -p .cache/$year
session=$(cat .cache/session)
if [[ -n "$AOC_SESSION" ]]; then
  session="$AOC_SESSION"
fi

if [[ "$3" = "--live" ]] || [[ "$1" = "--live" ]]; then
  termdown --no-figlet -T "$(printf 'AoC %d/%02d' $year $day)" -c 10 "$(date -d "$year-12-$day 00:00 EST")"
  while ! curl -sf -H "Cookie: session=$session" https://adventofcode.com/$year/day/$day/input -o .cache/$year/$day; do
    echo "Download failed, retrying..."
    sleep 1
  done
  less .cache/$year/$day
else
  curl -sf -H "Cookie: session=$session" https://adventofcode.com/$year/day/$day/input -o .cache/$year/$day

  p=1
  curl -sf -H "Cookie: session=$session" https://adventofcode.com/$year/day/$day | grep -P -o '<p>Your puzzle answer was <code>.*?</code>.</p>' | while read line; do
    if [[ "$line" =~ \<p\>Your\ puzzle\ answer\ was\ \<code\>(.*)\</code\>\.\</p\> ]]; then
      echo "${BASH_REMATCH[1]}" > .cache/$year/$day.$p
      p=2
    fi
  done
fi
