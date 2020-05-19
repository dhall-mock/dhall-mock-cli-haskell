{ nixpkgs ? import (import ./pinned-nixpkgs.nix) {} }:

nixpkgs.haskell.lib.buildStackProject {
    name = "dhall-mock-cli-haskell";
    src = builtins.fetchGit {
      url = "https://github.com/dhall-mock/dhall-mock-cli-haskell";
      ref = "master";
    };
    buildInputs = [ nixpkgs.zlib ];
    #shellHook = ''
    #  alias runDhallMock=${dhall-mock}/bin/main
    #  export DHALLMOCK_PID=$!
    #  #echo "dhall mock run with pid $DHALLMOCK_PID"
    #  #kill $DHALLMOCK_PID
    #'';
}
