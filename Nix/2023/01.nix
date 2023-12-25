let
  inherit (import <nixpkgs> {}) lib;
  input = builtins.readFile (builtins.getEnv "INPUT");
  lines = lib.splitString "\n" (lib.removeSuffix "\n" input);
  digits = line: map (s: builtins.fromJSON (builtins.head s)) (builtins.filter builtins.isList (builtins.split "([0-9])" line));
  nums = let
    words = ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"];
  in
    builtins.listToAttrs (builtins.genList (i: {
        name = builtins.elemAt words i;
        value = i + 1;
      }) (builtins.length words)
      ++ (builtins.genList (i: {
          name = toString i;
          value = i;
        })
        10));
  prefixes = s: let len = builtins.stringLength s; in builtins.genList (i: builtins.substring i len s) len;
  findWords = s: lib.flatten (map (p: builtins.filter (num: lib.hasPrefix num p) (builtins.attrNames nums)) (prefixes s));
  findNums = s: map (num: nums.${num}) (findWords s);
  sum = builtins.foldl' builtins.add 0;
  solve = digits: sum (map (d: builtins.head d * 10 + lib.last d) (map digits lines));
in {
  p1 = solve digits;
  p2 = solve findNums;
}
