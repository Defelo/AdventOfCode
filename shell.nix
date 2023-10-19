with import <nixpkgs> {};
  mkShell {
    buildInputs = [
      just

      # Haskell
      (haskellPackages.ghcWithPackages (p: with p; [regex-tdfa]))
      haskell-language-server
      ormolu # haskell code formatter
    ];
  }
