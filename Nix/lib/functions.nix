lib: let
  inherit (builtins) foldl';
in {
  flip = f: a: b: f b a;
  pipe = foldl' (acc: f: f acc);
}
