#!/usr/bin/env bash

while ! [[ -e flake.nix ]]; do cd ..; done
cd live

while true; do
  printf '\033[H\033[J\033[3J'
  python live.py
  inotifywait -qq live.py
done
