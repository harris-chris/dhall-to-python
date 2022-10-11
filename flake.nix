{
  description = "Utility to convert dhall records and types to Python";
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = github:numtide/flake-utils;
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = (final: prev: {
          inherit dhall-to-python;
        });
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        dhall-to-python = pkgs.haskellPackages.callCabal2nix "dhall-to-python" ./. {};
      in
      {
        packages = { inherit dhall-to-python; };
        defaultPackage = dhall-to-python;
        devShell = pkgs.haskellPackages.shellFor {
            name = "dhall-to-python";
            packages = p: [ dhall-to-python ];
            withHoogle = true;
            buildInputs = with pkgs; with pkgs.haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              hpack
              haskellPackages.zlib
            ];
          shellHook = "command -v fish &> /dev/null && fish";
          };
       }
    );
}
