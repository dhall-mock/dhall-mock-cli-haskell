let
  pkgs = import (import ./pinned-nixpkgs.nix) {};

  dhall-mock = import ./dhall-mock/default.nix;

in pkgs.stdenv.mkDerivation {
  name = "dhall-mock-cli-haskell-test";
  src = ../.;
  buildInputs = [ dhall-mock ];
  shellHook = ''
    ${dhall-mock}/bin/main &
    echo $!
    /usr/bin/stack test
  '';
}

#pkgs.stdenv.mkDerivation {
#  name = "dhall-mock-cli-haskell-test";
#  src = ./.;
#  buildInputs = [  ];
#  shellHook = ''echo coucou'';
#}

