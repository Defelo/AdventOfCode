rec {
  inherit (import <nixpkgs> {}) lib;

  scan = f: init: lst: let
    mkAcc = out: acc: {inherit out acc;};
    op = x: y: let
      acc' = f x.acc y;
    in
      mkAcc (x.out ++ [acc']) acc';
    result = builtins.foldl' op (mkAcc [] init) lst;
  in
    result.out;

  input = builtins.readFile (builtins.getEnv "INPUT");

  solution = {
    p1,
    p2 ? null,
  }:
    "${toString p1}\n" + (lib.optionalString (p2 != null) "${toString p2}\n");
}
