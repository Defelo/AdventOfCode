rec {
  inherit (import <nixpkgs> {}) lib;

  input = builtins.readFile (builtins.getEnv "INPUT");

  solution = {
    p1,
    p2 ? null,
  }:
    "${toString p1}\n" + (lib.optionalString (p2 != null) "${toString p2}\n");
}
