let
  nixpkgs-src = builtins.fetchTarball {
    name = "nixos-21.11";
    url = "https://github.com/nixos/nixpkgs/archive/e34c5379866.tar.gz";
    sha256 = "15shzr1wmc5770kblvlfwq5dsdlmvkpc3rhkn40nyi00fidqq96v";
  };

in
  import nixpkgs-src
