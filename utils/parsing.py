import re


def ints(inp):
    return [int(x[0]) for x in re.finditer(r"[+-]?\d+", inp)]


def pints(inp):
    return [int(x[0]) for x in re.finditer(r"\d+", inp)]


def floats(inp):
    return [float(x[0]) for x in re.finditer(r"[+-]?(\d*\.)?\d+", inp)]


def pfloats(inp):
    return [float(x[0]) for x in re.finditer(r"(\d*\.)?\d+", inp)]
