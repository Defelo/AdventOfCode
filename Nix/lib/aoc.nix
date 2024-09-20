lib: let
  inherit (builtins) readFile getEnv;
  inherit (lib.strings) optionalString;
in {
  input = readFile (getEnv "INPUT");

  solution = args: args // {__toString = s: "${toString s.p1}\n" + (optionalString (s ? p2) "${toString s.p2}\n");};
}
