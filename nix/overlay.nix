_: pkgs:

let
  overrides = _: hspkgs: with pkgs.haskell.lib;
    let
      call = name: path: hspkgs.callCabal2nix name path {};
    in
      {
        # hoauth2 = dontCheck (call "hoauth2" hoauth2-src);
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
