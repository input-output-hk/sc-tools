{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  utils = import ./utils.nix { inherit pkgs lib; };

  # Each GHC version gets its own project - evaluated lazily
  projects = import ./project.nix { inherit inputs pkgs lib; };

  # Create shells using the projects attribute set
  mkShell = { ghc, withHoogle ? true }:
    import ./shell.nix { inherit inputs pkgs lib projects utils ghc system withHoogle; };

  packages = { };

  devShells = rec {
    default = ghc9103;
    ghc966 = mkShell { ghc = "ghc966"; };
    ghc966-nohoogle = mkShell { ghc = "ghc966"; withHoogle = false; };
    ghc9103 = mkShell { ghc = "ghc9103"; };
    ghc9103-nohoogle = mkShell { ghc = "ghc9103"; withHoogle = false; };
  };

  # Hydra jobs - each GHC version's flake is evaluated separately
  defaultHydraJobs = {
    ghc966 = (projects.ghc966.flake {}).hydraJobs;
    ghc9103 = (projects.ghc9103.flake {}).hydraJobs;
    inherit packages;
    inherit devShells;
    required = utils.makeHydraRequiredJob hydraJobs;
  };

  hydraJobsPerSystem = {
    "x86_64-linux"   = defaultHydraJobs;
    "x86_64-darwin"  = defaultHydraJobs;
    "aarch64-linux"  = defaultHydraJobs;
    "aarch64-darwin" = defaultHydraJobs;
  };

  hydraJobs = utils.flattenDerivationTree "-" hydraJobsPerSystem.${system};

  # Default project flake for apps/packages (uses ghc9103)
  defaultProjectFlake = projects.ghc9103.flake {};
in

{
  inherit devShells;
  hydraJobs = hydraJobs;
  apps = defaultProjectFlake.apps;
  packages = defaultProjectFlake.packages;
  # Explore the projects via nix repl '.#'
  inherit projects;
}
