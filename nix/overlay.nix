_: pkgs:

let
  wai-middleware-auth-src = pkgs.fetchFromGitHub {
    owner = "fpco";
    repo = "wai-middleware-auth";
    rev = "c9544ab9ad631c612f7940340c3d9c56a0c219dc";
    sha256 = "15rlx3yp1gb4bm23ykz46jy5yqz00dh0rlm7c6wafd48iyvjm65d";
  };

  hoauth2-src = pkgs.fetchFromGitHub {
    owner = "guaraqe";
    repo = "hoauth2";
    rev = "9b81fb3cd759354bc7fc7de80d4dd111f1285dcf";
    sha256 = "1qar92r0jk01nvmzk8hk6j3hx3jrzscyv18yqi8p22chk7d0hnk0";
  };

  overrides = _: hspkgs: with pkgs.haskell.lib;
    let
      call = name: path: hspkgs.callCabal2nix name path {};
    in
      {
        hoauth2 =
          dontCheck (call "hoauth2" hoauth2-src);

        wai-middleware-auth =
          dontCheck (call "wai-middleware-auth" wai-middleware-auth-src);
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
