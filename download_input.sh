#!/usr/bin/env bash

prog=$(realpath "$0")
cd $(dirname "$prog")

year=$(date +%Y)
month=$(date +%m)
day=$(date +%d)

if [[ $# -eq 1 ]] && [[ $1 = "--all" ]]; then
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
elif [[ $# -eq 2 ]]; then
  year=$1
  day=$2
elif [[ $# -ne 0 ]] || [[ $month -ne 12 ]] || [[ $day -gt 25 ]]; then
  echo "usage: $0 <year> <day>"
  exit 1
fi

echo $year/$day

mkdir -p .cache/$year
session=$(cat .cache/session)
curl -sf -H "Cookie: session=$session" https://adventofcode.com/$year/day/$day/input -o .cache/$year/$day

p=1
curl -sf -H "Cookie: session=$session" https://adventofcode.com/$year/day/$day | grep -P -o '<p>Your puzzle answer was <code>.*?</code>.</p>' | while read line; do
  if [[ "$line" =~ \<p\>Your\ puzzle\ answer\ was\ \<code\>(.*)\</code\>\.\</p\> ]]; then
    echo "${BASH_REMATCH[1]}" > .cache/$year/$day.$p
    p=2
  fi
done
