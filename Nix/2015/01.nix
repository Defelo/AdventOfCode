let
  lib = import ../lib;
  inherit (builtins) getAttr add;
  inherit (lib.aoc) input solution;
  inherit (lib.functions) pipe flip;
  inherit (lib.lists) scan last findFirstIndex;
  inherit (lib.strings) stringToCharacters trim;

  chars = {
    "(" = 1;
    ")" = -1;
  };
  levels = pipe input [
    trim
    stringToCharacters
    (map (flip getAttr chars))
    (scan add 0)
  ];
in
  solution {
    p1 = last levels;
    p2 = findFirstIndex (x: x == -1) null levels + 1;
  }
