let
  nixpkgs-src = builtins.fetchTarball {
    name = "nixos-20.09";
    url = "https://github.com/nixos/nixpkgs/archive/1c1f5649bb9c1b0d98637c8c365228f57126f361.tar.gz";
    sha256 = "0f2nvdijyxfgl5kwyb4465pppd5vkhqxddx6v40k2s0z9jfhj0xl";
  };

in
  import nixpkgs-src
