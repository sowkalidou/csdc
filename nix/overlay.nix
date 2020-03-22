_: pkgs:

let
  filter = src: pkgs.nix-gitignore.gitignoreSource [] src;

  overrides = _: hspkgs:
    let
      call = name: path: hspkgs.callCabal2nix name (filter path) {};
    in
      {
        csdc-base = call "csdc-base" ../csdc-base;
        csdc-api = call "csdc-api" ../csdc-api;
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
