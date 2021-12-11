_: pkgs:

let
  hoauth2-src = pkgs.fetchFromGitHub {
    owner = "guaraqe";
    repo = "hoauth2";
    rev = "4169dfb71a955db1471800c1774d230ed1f999d1";
    sha256 = "ctWEUN/sH5PzQvE3gaoROY5KaAQHzsJFWkImeG3uhvU=";
  };

  overrides = _: hspkgs: with pkgs.haskell.lib;
    let
      call = name: path: hspkgs.callCabal2nix name path {};
    in
      {
        hoauth2 =
          dontCheck (call "hoauth2" hoauth2-src);
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
