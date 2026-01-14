{ inputs, pkgs, lib }:

let
  cabalProject = pkgs.haskell-nix.cabalProject' (
    
    { config, pkgs, ... }:

    {
      name = "sc-tools";

      compiler-nix-name = lib.mkDefault "ghc966";

      src = lib.cleanSource ../.;

      flake.variants = {
        ghc966 = {}; # Alias for the default variant
        #ghc984.compiler-nix-name = "ghc984";
        #ghc9102.compiler-nix-name = "ghc9102";
        #ghc9122.compiler-nix-name = "ghc9122";
      };

      inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };

      cabalProjectLocal = ''
        package *
          ghc-options=-Werror
      '';
      modules = [{
        packages = {
          convex-base.ghcOptions = [ "-Werror" ];
          convex-blockfrost.ghcOptions = [ "-Werror" ];
          convex-coin-selection.ghcOptions = [ "-Werror" ];
          convex-devnet.ghcOptions = [ "-Werror" ];
          convex-maestro.ghcOptions = [ "-Werror" ];
          convex-mockchain.ghcOptions = [ "-Werror" ];
          convex-node-client.ghcOptions = [ "-Werror" ];
          convex-optics.ghcOptions = [ "-Werror" ];
          convex-tx-mod.ghcOptions = [ "-Werror" ];
          convex-wallet.ghcOptions = [ "-Werror" ];
        };
      }      
      ];
    }
  );

in

cabalProject

