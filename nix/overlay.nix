_: pkgs:

let
  overrides = _: hspkgs: with pkgs.haskell.lib;
    {
      ipfs = (dontCheck hspkgs.ipfs).overrideAttrs (old: {
        meta = old.meta // { broken = false; };
      });
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
