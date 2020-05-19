let
  pkgs = import (import ../pinned-nixpkgs.nix) { 
  };

  mkRustPlatform = pkgs.callPackage ./mk-rust-platform.nix {};

  rustPlatform = mkRustPlatform {
    date = "2020-05-15";
    channel = "nightly";
  };

in rustPlatform.buildRustPackage rec {
  pname = "dhall-mock";
  version = "0.1.0";

  src = builtins.fetchGit {
    url = "https://github.com/dhall-mock/dhall-mock";
    ref = "master";
  };

  cargoSha256 = "0g987i489zxwmm8kmx73dbrxc551ymk0qfnjrf1v203jk7r48m1y";

  meta = with pkgs.stdenv.lib; {
    description = "A mock server";
    homepage = "https://github.com/dhall-mock/dhall-mock";
    license = licenses.unlicense;
    maintainers = [];
    platforms = platforms.linux;
  };

  nativeBuildInputs = [ pkgs.pkgconfig ];

  buildInputs = [ pkgs.openssl.dev ];
}
