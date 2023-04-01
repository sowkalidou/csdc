_: pkgs:

let
  overrides = _: hspkgs: with pkgs.haskell.lib;
    {
      ipfs = hspkgs.callCabal2nix "ipfs" (pkgs.fetchFromGitHub {
        repo = "ipfs-haskell";
        owner = "guaraqe";
        rev = "ad21c0815818f9c4e0113bcaef67f026adc6079c";
        sha256 = "sha256-4FyHJ5o/aRzdewy+2RfilwzqicCyebfoql5e/2/WjIk=";
      }) {};
    };
in

{
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions
        (old.overrides or (_: _: {}))
        overrides;
  });
}
