{
  description = "Overengineered Hangman example";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghc = "ghc948";
        pkgs = import nixpkgs { inherit system; };
        slim-exec = pkg: pkgs.haskell.lib.justStaticExecutables pkg;
        haskellPkgs = pkgs.haskell.packages.${ghc}.override {
            overrides = ghcSelf: ghcSuper: {
                generics-sop = pkgs.haskell.lib.doJailbreak ghcSuper.generics-sop;
                hangman = ghcSuper.callCabal2nix "hangman" ./hangman {};
                hangman-adapters = ghcSuper.callCabal2nix "hangman-adapters" ./hangman-adapters {};
                hangman-server = slim-exec (ghcSuper.callCabal2nix "hangman-server" ./hangman-server {});
            };
        };
        server-image = pkgs.dockerTools.buildImage {
          name = "hangman-server";
          tag = self.rev or "dirty";
          copyToRoot = [ hangman.server ];
          config = {
            Cmd = [ "hangman-server-exe" "-p 8080" ];
          };
        };
        hangman = {
            lib = haskellPkgs.hangman;
            adapters = haskellPkgs.hangman-adapters;
            server = haskellPkgs.hangman-server;
            server-docker = server-image;
        };
      in
        {
          packages = {
            inherit hangman;
            default = hangman.server;
          };

          devShells.default = haskellPkgs.shellFor {
            packages = p: [hangman.lib hangman.adapters hangman.server];

            buildInputs = with pkgs.haskellPackages; [
              haskell-language-server
              cabal-install
            ];

            withHoogle = false;
          };
        }
  );
}
