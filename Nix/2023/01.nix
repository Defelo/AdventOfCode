let
  lib = import ../lib;
  inherit (builtins) fromJSON head filter isList split listToAttrs length genList elemAt stringLength substring attrNames foldl' add;
  inherit (lib.aoc) input solution;
  inherit (lib.lists) flatten last;
  inherit (lib.strings) splitLines trim hasPrefix;

  lines = splitLines (trim input);

  digits = line: map (s: fromJSON (head s)) (filter isList (split "([0-9])" line));
  nums = let
    words = ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"];
    wordList = genList (i: {
      name = elemAt words i;
      value = i + 1;
    }) (length words);
    digitList =
      genList (i: {
        name = toString i;
        value = i;
      })
      10;
  in
    listToAttrs (wordList ++ digitList);

  prefixes = s: let len = stringLength s; in genList (i: substring i len s) len;
  findWords = s: flatten (map (p: filter (num: hasPrefix num p) (attrNames nums)) (prefixes s));
  findNums = s: map (num: nums.${num}) (findWords s);
  sum = foldl' add 0;
  solve = digits: sum (map (d: head d * 10 + last d) (map digits lines));
in
  solution {
    p1 = solve digits;
    p2 = solve findNums;
  }
