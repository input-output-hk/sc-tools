{ inputs, pkgs, lib, projects, utils, ghc, system, withHoogle ? true }:

let

  cardano-node = inputs.cardano-node.packages."${system}".cardano-node;
  cardano-cli  = inputs.cardano-cli.legacyPackages."${system}".cardano-cli;

  # Get the project for this specific GHC version (lazy evaluation)
  project = projects.${ghc};

  # Tools are looked up from the specific GHC project
  tools = {
    cabal                   = project.tool "cabal" "latest";
    cabal-fmt               = projects.ghc966.tool "cabal-fmt" "latest"; # cabal-fmt only buildable with ghc966
    haskell-language-server = project.tool "haskell-language-server" "latest";
    stylish-haskell         = project.tool "stylish-haskell" "latest";
    fourmolu                = project.tool "fourmolu" "latest";
    hlint                   = project.tool "hlint" "latest";
  };

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSources ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = false;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = true;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
      };
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
    };
  };

  linuxPkgs = lib.optionals pkgs.hostPlatform.isLinux [
  ];

  darwinPkgs = lib.optionals pkgs.hostPlatform.isDarwin [
  ];

  commonPkgs = [
    tools.haskell-language-server
    tools.stylish-haskell
    tools.fourmolu
    tools.cabal
    tools.hlint
    tools.cabal-fmt

    pkgs.shellcheck
    pkgs.nixpkgs-fmt
    pkgs.github-cli
    pkgs.act
    pkgs.bzip2
    pkgs.gawk
    pkgs.zlib
    pkgs.cacert
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.which
    cardano-node
    cardano-cli
  ];

  shell = project.shellFor {
    name = "sc-tools-${ghc}";

    buildInputs = lib.concatLists [
      commonPkgs
      darwinPkgs
      linuxPkgs
    ];

    withHoogle = withHoogle;

    shellHook = ''
      ${preCommitCheck.shellHook}
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
      export CARDANO_NODE="${cardano-node}/bin/cardano-node";
      export CARDANO_CLI="${cardano-cli}/bin/cardano-cli";
    '';
  };

in

shell
