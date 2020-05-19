let
  pkgs = import (import ./pinned-nixpkgs.nix) {};

  dhall-mock = import ./dhall-mock/default.nix;

in pkgs.stdenv.mkDerivation {
  name = "dhall-mock-cli-haskell-test";
  buildInputs = [ dhall-mock pkgs.zlib ];
  
  src = builtins.fetchGit {
    url = "https://github.com/dhall-mock/dhall-mock-cli-haskell";
    ref = "master";
  };
  #shellHook = ''
  #  ${dhall-mock}/bin/main
  #'';
}

#pkgs.stdenv.mkDerivation {
#  name = "dhall-mock-cli-haskell-test";
#  src = ./.;
#  buildInputs = [  ];
#  shellHook = ''echo coucou'';
#}

