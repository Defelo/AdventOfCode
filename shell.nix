with import <nixpkgs> {}; let
  downloadInput = stdenvNoCC.mkDerivation {
    name = "aoc-download-input";
    dontUnpack = true;
    nativeBuildInputs = [makeWrapper];
    installPhase = ''
      mkdir -p $out/bin
      cp ${./scripts/download_input.sh} $out/bin/aoc-download-input
      chmod +x $out/bin/*
      wrapProgram $out/bin/* --set PATH ${lib.makeBinPath [
        bash
        coreutils
        curl
        gnugrep
      ]}
    '';
  };
  getSession = python3.pkgs.buildPythonApplication {
    name = "aoc-get-session";
    pyproject = false;
    dontUnpack = true;
    installPhase = "mkdir -p $out/bin; cp ${./scripts/get_session.py} $out/bin/aoc-get-session; chmod +x $out/bin/*";
    propagatedBuildInputs = with python3.pkgs; [
      requests
      pycrypto
    ];
  };
in
  mkShell {
    buildInputs = [
      downloadInput
      getSession
    ];
    packages = [
      just

      # Python
      (python311.withPackages (p:
        with p; [
          numpy
          pyperclip
        ]))

      # Haskell
      (haskellPackages.ghcWithPackages (p: with p; [regex-tdfa]))
      haskell-language-server
      ormolu # haskell code formatter
    ];
    PYTHONPATH = ".";
  }
