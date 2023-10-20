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
import heapq
import io
import itertools
import json
import math
import operator
import re
import statistics
from collections import Counter
from copy import deepcopy
from functools import cache, partial, reduce
from heapq import heapify, heappop, heappush
from pathlib import Path

import numpy as np
import pyperclip


def read_input(year: int, day: int) -> str:
    f = Path(__file__).parent / f"../../.cache/{year}/{day}"
    return f.read_text()


def ans(answer):
    print(answer)
    pyperclip.copy(str(answer))
