from IPython.display import display, Markdown
import requests
import os

SESSION = "your session cookie"

def load(year, day, strip=True):
    f = f"{day:02}.txt"
    if not os.path.exists(f):
        puzzle = _fetch_input(year, day)
        if puzzle is None:
            raise Exception("Puzzle input could not be fetched!")
        _create_file(f, puzzle)
    else:
        puzzle = open(f).read()
    
    if strip:
        puzzle = puzzle.strip()
    
    return puzzle

def setup(year, day, strip=True):
    display(Markdown(f"# Day {day:02}"))
    return load(year, day, strip)

def _create_file(path, content, debug=False):
    if debug:
        print(f"Creating file {path}")
    with open(path, "w") as file:
        file.write(content)
        file.flush()

def _fetch_input(year, day, debug=False):
    if debug:
        print(f"Fetching input for {year}/{day:02}")
    response = requests.get(f"https://adventofcode.com/{year}/day/{day}/input", cookies={"session": SESSION})
    return response.text if response.ok else None
