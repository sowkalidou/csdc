with import ./nix;
with builtins;
let
  # Set of package sources parsed from cabal.project.
  targets = import ./nix/cabal.nix;

  # Add local packages to haskellPackages.
  localHaskellPackages =
    haskellPackages.extend (haskell.lib.packageSourceOverrides targets);

  # Set of local packages, built from targets.
  packages =
    mapAttrs (name: _: localHaskellPackages.${name}) targets;
in
  packages // {
    csdc-gui = pkgs.callPackage ./csdc-gui {};
  }
