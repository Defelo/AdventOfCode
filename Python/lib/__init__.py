from .graph import *
from .grid import *
from .lists import *
from .math import *
from .parsing import *

del graph, grid, lists, math, parsing

import ast
import bisect
import collections
import functools
import graphlib
import hashlib
import heapq
import io
import itertools
import json
import math
import operator
import re
import statistics
import string
from collections import Counter, deque
from copy import deepcopy
from dataclasses import dataclass
from datetime import date, datetime, time, timedelta
from functools import cache, partial, reduce
from heapq import heapify, heappop, heappush
from pathlib import Path

import numpy as np
import pyperclip
import z3

YEAR: int | None = None
DAY: int | None = None


def read_input(year: int, day: int, example: int | None = None) -> str:
    global YEAR, DAY
    YEAR = year
    DAY = day
    if example is None:
        f = Path(__file__).parent / f"../../.cache/{year}/{day}"
    else:
        f = Path(__file__).parent / f"../../examples/{year}/{day}/{example}"
    return f.read_text()


def ans(answer):
    print(answer)
    pyperclip.copy(str(answer))

    submit_part = input("\nSubmit solution? level=")
    if submit_part in ["1", "2"] and YEAR is not None and DAY is not None:
        import bs4
        import requests

        session = (Path(__file__).parent / f"../../.cache/session").read_text()
        resp = requests.post(
            f"https://adventofcode.com/{YEAR}/day/{DAY}/answer",
            cookies={"session": session},
            data={"level": submit_part, "answer": answer},
        ).text
        bs = bs4.BeautifulSoup(resp, "html.parser")
        print(bs.main.article.p.text)
