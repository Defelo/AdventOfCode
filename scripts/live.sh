#!/usr/bin/env bash

set -m

while ! [[ -e flake.nix ]]; do cd ..; done
cd live

while true; do
  printf '\033[H\033[J\033[3J\033[37m'
  python live.py &
  py=$!
  (inotifywait -qqe modify live.py; kill $py) &
  wa=$!
  fg %-
  wait $wa
done
