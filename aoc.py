from pathlib import Path

SESSION = Path(__file__).parent.joinpath(".session.txt").read_text().strip()


def load(year, day, strip=True, f=None):
    if not f:
        f = Path(__file__).parent / f"{year}/{day:02}.txt"
    if not f.exists():
        puzzle = _fetch_input(year, day)
        if puzzle is None:
            raise Exception("Puzzle input could not be fetched!")
        f.write_text(puzzle)
    else:
        puzzle = f.read_text()

    if strip:
        puzzle = puzzle.strip()

    return puzzle


def setup(year, day, strip=True, f=None):
    return load(year, day, strip, Path(f) if isinstance(f, str) else f)


def create_file(path, content, debug=False):
    if debug:
        print(f"Creating file {path}")
    with open(path, "w") as f:
        f.write(content)
        f.flush()


def _fetch_input(year, day, debug=False):
    import requests

    if debug:
        print(f"Fetching input for {year}/{day:02}")
    response = requests.get(f"https://adventofcode.com/{year}/day/{day}/input", cookies={"session": SESSION})
    return response.text if response.ok else None
