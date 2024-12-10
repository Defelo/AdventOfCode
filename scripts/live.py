#!/usr/bin/env python


import os
import subprocess
import sys
import time
from multiprocessing import Process
from pathlib import Path
from threading import Thread
from typing import Any, cast

import bs4
import pyperclip
import requests
from watchdog.events import FileSystemEvent, FileSystemEventHandler
from watchdog.observers import Observer

root_dir = Path(os.getcwd())
while not (root_dir / "flake.nix").is_file():
    root_dir = root_dir.parent
os.chdir(root_dir / "live")

if len(sys.argv) != 3:
    print(f"usage: {sys.argv[0]} YEAR DAY")
    exit(1)

year, day = map(int, sys.argv[1:3])


def run_solution(input: Path) -> str | None:
    if not input.exists():
        print(f"\033[1m\033[31mInput file {input} does not exist\033[0m")
        return None
    py = os.environ.get("AOC_PYTHON", "python")
    inp = input.read_bytes()
    print(f"cmd: {py} live.py | input: {input.resolve().relative_to(root_dir)}")
    start = time.time()
    out = subprocess.run([py, "live.py"], input=inp, capture_output=True)
    delta = time.time() - start
    print(f"\033[3{'12'[out.returncode == 0]}mexit code: {out.returncode} | delta: {delta:.2f}s\033[0m")
    print("--- stderr ---")
    sys.stdout.buffer.write(out.stderr)
    print("--- stdout ---")
    sys.stdout.buffer.write(out.stdout)

    if out.returncode != 0:
        return None

    try:
        return out.stdout.decode().strip().splitlines()[-1].strip()
    except:
        return None


def trigger():

    print(end="\033[H\033[2J\033[0m")
    ex_dir = Path(f"../examples/{year}/{day}")
    for ex in sorted(
        (x for x in (ex_dir.iterdir() if ex_dir.is_dir() else []) if x.name.isnumeric()), key=lambda f: int(f.name)
    ):
        n = int(ex.name)
        print(f"\033[1m\033[34m----- Example {n} -----\033[0m")
        run_solution(ex)
        print()

    print("\033[1m\033[34m----- Puzzle Input -----\033[0m")
    ans = run_solution(Path(f"../.cache/{year}/{day}"))
    if ans is not None:
        print(f"\n\033[1m\033[32mAnswer: {ans}\033[0m")
        pyperclip.copy(ans)
        if (part := input(f"Submit? level=")) in ["1", "2"]:
            print(f"(submitting answer for part {part})")
            session = (root_dir / ".cache/session").read_text().strip()
            resp = requests.post(
                f"https://adventofcode.com/{year}/day/{day}/answer",
                cookies={"session": session},
                data={"level": part, "answer": ans},
            ).text
            bs = bs4.BeautifulSoup(resp, "html.parser")
            resp = cast(Any, bs).main.article.p.text
            ok = resp.startswith("That's the right answer!")
            print(f"\033[1m\033[3{'12'[ok]}m{resp}\033[0m")
    else:
        print("\033[1m\033[31m(failed to find answer in program output)\033[0m")
    print("(waiting for changes to live.py)")


proc: Process | None = None


def spawn_trigger_process():
    global proc

    if proc is not None and proc.is_alive():
        print("(process killed)")
        proc.kill()

    def trigger_wrapper():
        sys.stdin = open(0)
        while True:
            trigger()
            input()

    proc = Process(target=trigger_wrapper)
    proc.start()


class Handler(FileSystemEventHandler):
    def __init__(self):
        super().__init__()
        self.cnt = 0

    def on_modified(self, event: FileSystemEvent) -> None:
        if event.src_path != "./live.py":
            return

        self.cnt += 1
        cnt = self.cnt

        def inner():
            time.sleep(0.1)
            if self.cnt == cnt:
                spawn_trigger_process()

        t = Thread(target=inner)
        t.start()


spawn_trigger_process()

handler = Handler()
observer = Observer()
observer.schedule(handler, ".", recursive=True)
observer.start()

while True:
    time.sleep(1)
