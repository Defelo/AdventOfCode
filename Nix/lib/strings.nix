lib: let
  inherit (builtins) match head substring genList stringLength replaceStrings split filter isString;
in rec {
  optionalString = x: s:
    if x
    then s
    else "";

  trim = s: let
    chars = " \t\r\n";
    regex = "[${chars}]*(.*[^${chars}])[${chars}]*";
    res = match regex s;
  in
    optionalString (res != null) (head res);

  charAt = s: i: substring i 1 s;
  stringToCharacters = s: genList (charAt s) (stringLength s);

  escape = lst: replaceStrings lst (map (c: "\\${c}") lst);
  escapeRegex = escape (stringToCharacters "\\[{()^$?*+|.");

  splitString = sep: s: filter isString (split (escapeRegex sep) s);
  splitLines = splitString "\n";

  hasPrefix = p: s: substring 0 (stringLength p) s == p;
}
