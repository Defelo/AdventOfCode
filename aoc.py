from typing import Any, Callable
from pathlib import Path

SESSION = Path(__file__).parent.joinpath(".session.txt").read_text().strip()


def run(
    year: int,
    day: int,
    part1: Callable[[str], Any],
    part2: Callable[[str], Any],
    strip: str | bool = "\n",
    f: Path | None = None,
):
    puzzle = load(year, day, strip, f)
    print("part 1:", part1(puzzle))
    print("part 2:", part2(puzzle))


def load(year: int, day: int, strip: str | bool = "\n", f: Path | None = None) -> str:
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
        puzzle = puzzle.strip() if strip is True else puzzle.strip(strip)

    return puzzle


def setup(year: int, day: int, strip: str | bool = "\n", f: str | None = None) -> str:
    return load(year, day, strip, Path(f) if isinstance(f, str) else f)


def create_file(path: str, content: str, debug: bool = False) -> None:
    if debug:
        print(f"Creating file {path}")
    with open(path, "w") as f:
        f.write(content)
        f.flush()


def _fetch_input(year: int, day: int, debug: bool = False) -> str | None:
    import requests

    if debug:
        print(f"Fetching input for {year}/{day:02}")
    response = requests.get(f"https://adventofcode.com/{year}/day/{day}/input", cookies={"session": SESSION})
    return response.text if response.ok else None
