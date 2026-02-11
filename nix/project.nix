{ inputs, pkgs, lib }:

let
  # Common project configuration shared across GHC versions
  commonConfig = {
    name = "sc-tools";

    src = lib.cleanSource ../.;

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
    }];
  };

  # Create a project for a specific GHC version (lazy - only evaluated when accessed)
  mkProject = compiler-nix-name:
    pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }:
      commonConfig // {
        inherit compiler-nix-name;
      }
    );

in

{
  # Each project is created lazily - only evaluated when accessed
  ghc966 = mkProject "ghc966";
  ghc9103 = mkProject "ghc9103";
  #ghc984 = mkProject "ghc984";
  #ghc9122 = mkProject "ghc9122";
}
