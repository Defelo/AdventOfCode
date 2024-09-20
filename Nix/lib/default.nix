let
  pipe = builtins.foldl' (acc: f: f acc);

  removeDotNix = s: let
    removed = builtins.match "(.*)\\.nix" s;
  in
    if removed != null
    then builtins.head removed
    else s;

  lib = pipe ./. [
    builtins.readDir
    builtins.attrNames
    (builtins.filter (name: name != "default.nix"))
    (map (name: {
      name = removeDotNix name;
      value = import ./${name} lib;
    }))
    builtins.listToAttrs
  ];
in
  lib
