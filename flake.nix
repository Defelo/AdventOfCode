{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    uiua.url = "github:uiua-lang/uiua";
  };

  outputs = {
    nixpkgs,
    uiua,
    ...
  }: let
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

            python = pkgs.python311.withPackages (p:
              with p; [
                z3
                numpy
                pyperclip
                requests
                beautifulsoup4
                networkx
                sympy
              ]);

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
                    termdown
                    less
                    glibc
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
            pruneLeaderboard = pkgs.python3.pkgs.buildPythonApplication {
              name = "aoc-prune-leaderboard";
              pyproject = false;
              dontUnpack = true;
              installPhase = "mkdir -p $out/bin; cp ${./scripts/prune_leaderboard.py} $out/bin/aoc-prune-leaderboard; chmod +x $out/bin/*";
              propagatedBuildInputs = with pkgs.python3.pkgs; [
                requests
              ];
            };
            fetchRanks = pkgs.python3.pkgs.buildPythonApplication {
              name = "aoc-fetch-ranks";
              pyproject = false;
              dontUnpack = true;
              installPhase = "mkdir -p $out/bin; cp ${./scripts/fetch_ranks.py} $out/bin/aoc-fetch-ranks; chmod +x $out/bin/*";
              propagatedBuildInputs = with pkgs.python3.pkgs; [
                requests
                beautifulsoup4
              ];
            };
            live = pkgs.stdenvNoCC.mkDerivation {
              name = "aoc-live";
              dontUnpack = true;
              nativeBuildInputs = with pkgs; [makeWrapper];
              installPhase = ''
                mkdir -p $out/bin
                cp ${./scripts/live.sh} $out/bin/aoc-live
                chmod +x $out/bin/*
                wrapProgram $out/bin/* --set PATH ${with pkgs;
                  lib.makeBinPath [
                    python
                    bash
                    coreutils
                    inotify-tools
                    wl-clipboard
                  ]}
              '';
            };
          };
        })
        defaultSystems);
  in {
    devShells = eachDefaultSystem ({
      system,
      pkgs,
      downloadInput,
      getSession,
      pruneLeaderboard,
      fetchRanks,
      live,
      python,
      ...
    }: {
      default = pkgs.mkShell {
        buildInputs = [
          downloadInput
          getSession
          pruneLeaderboard
          fetchRanks
          live
          pkgs.z3
        ];
        packages = with pkgs; [
          just
          jq
          hyperfine

          # Python
          python

          # Haskell
          (haskellPackages.ghcWithPackages (p: with p; [split regex-tdfa]))
          haskell-language-server
          ormolu # haskell code formatter

          # APL
          (dyalog.override {
            acceptLicense = true;
          })

          # Uiua
          uiua.packages.${system}.default
        ];
        PYTHONPATH = ".";
        LIBCLANG_PATH = with pkgs; lib.makeLibraryPath [llvmPackages.clang-unwrapped.lib];
        LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath [z3.lib];
        CPATH = with pkgs; lib.makeSearchPath "include" [musl.dev llvmPackages.clang-unwrapped.lib z3.dev];
      };
    });
  };
}
