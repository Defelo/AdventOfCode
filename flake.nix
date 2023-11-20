{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = {nixpkgs, ...}: let
    defaultSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];
    eachDefaultSystem = f:
      builtins.listToAttrs (map (system: {
          name = system;
          value = f rec {
            inherit system;
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfreePredicate = pkg:
                builtins.elem (nixpkgs.lib.getName pkg) [
                  "dyalog"
                ];
            };

            downloadInput = pkgs.stdenvNoCC.mkDerivation {
              name = "aoc-download-input";
              dontUnpack = true;
              nativeBuildInputs = with pkgs; [makeWrapper];
              installPhase = ''
                mkdir -p $out/bin
                cp ${./scripts/download_input.sh} $out/bin/aoc-download-input
                chmod +x $out/bin/*
                wrapProgram $out/bin/* --set PATH ${with pkgs;
                  lib.makeBinPath [
                    bash
                    coreutils
                    curl
                    gnugrep
                  ]}
              '';
            };
            getSession = pkgs.python3.pkgs.buildPythonApplication {
              name = "aoc-get-session";
              pyproject = false;
              dontUnpack = true;
              installPhase = "mkdir -p $out/bin; cp ${./scripts/get_session.py} $out/bin/aoc-get-session; chmod +x $out/bin/*";
              propagatedBuildInputs = with pkgs.python3.pkgs; [
                requests
                pycrypto
              ];
            };
          };
        })
        defaultSystems);
  in {
    devShells = eachDefaultSystem ({
      pkgs,
      downloadInput,
      getSession,
      ...
    }: {
      default = pkgs.mkShell {
        buildInputs = [
          downloadInput
          getSession
        ];
        packages = with pkgs; [
          just
          jq
          hyperfine

          # Python
          (python311.withPackages (p:
            with p; [
              z3
              numpy
              pyperclip
            ]))

          # Haskell
          (haskellPackages.ghcWithPackages (p: with p; [regex-tdfa]))
          haskell-language-server
          ormolu # haskell code formatter

          # APL
          (dyalog.override {
            acceptLicense = true;
          })

          # Uiua
          uiua
        ];
        PYTHONPATH = ".";
      };
    });
  };
}
