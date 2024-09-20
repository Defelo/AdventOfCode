lib: let
  inherit (builtins) elemAt length head tail isList concatMap;
in rec {
  last = lst: elemAt lst (length lst - 1);

  findFirstIndex = pred: default: lst: let
    find = lst:
      if lst == []
      then null
      else if pred (head lst)
      then 0
      else let
        x = find (tail lst);
      in
        if x == null
        then null
        else x + 1;
    result = find lst;
  in
    if result == null
    then default
    else result;

  scan = f: init: lst: let
    mkAcc = out: acc: {inherit out acc;};
    op = x: y: let
      acc' = f x.acc y;
    in
      mkAcc (x.out ++ [acc']) acc';
    result = builtins.foldl' op (mkAcc [] init) lst;
  in
    result.out;

  flatten = lst:
    if isList lst
    then concatMap flatten lst
    else [lst];
}
