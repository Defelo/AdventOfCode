let
  lib = import ../lib;
  inherit (builtins) match fromJSON head sort lessThan length filter;
  inherit (lib.aoc) input solution;
  inherit (lib.lists) last zipWith sum;
  inherit (lib.functions) flip pipe;
  inherit (lib.math) abs;
  inherit (lib.strings) splitLines trim;

  sorted = sort lessThan;

  parsed = pipe input [
    trim
    splitLines
    (map (flip pipe [
      (match " *([0-9]+) *([0-9]+) *")
      (map fromJSON)
    ]))
  ];

  left = map head parsed;
  right = map last parsed;

  count = x: xs: length (filter (a: a == x) xs);
in
  solution {
    p1 = sum (zipWith (a: b: abs (a - b)) (sorted left) (sorted right));
    p2 = sum (map (l: l * (count l right)) left);
  }
