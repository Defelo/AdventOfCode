let
  inherit (import ../lib) lib input solution scan;
  inherit (lib) pipe trim stringToCharacters flip getAttr last;
  inherit (lib.lists) findFirstIndex;

  chars = {
    "(" = 1;
    ")" = -1;
  };
  levels = pipe input [
    trim
    stringToCharacters
    (map (flip getAttr chars))
    (scan (acc: x: acc + x) 0)
  ];
in
  solution {
    p1 = last levels;
    p2 = findFirstIndex (x: x == -1) null levels + 1;
  }
