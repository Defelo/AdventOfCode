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


def read_input(year: int, day: int, example: int | None = None) -> str:
    if example is None:
        f = Path(__file__).parent / f"../../.cache/{year}/{day}"
    else:
        f = Path(__file__).parent / f"../../examples/{year}/{day}/{example}"
    return f.read_text()


def ans(answer):
    print(answer)
    pyperclip.copy(str(answer))
