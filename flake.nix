{
  description = "Overengineered Hangman example";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghc = "ghc963";
        gitRev = self.rev or "dirty";
        overlays = final: prev: {
            elfutils = prev.elfutils.override {
                # So that we won't end up with python3 in docker image...
                # GHC (since 9.6.3) depends on elfutils -
                # Elfutils with enableDebuginfod set to true (default) brings curl
                # curl brings libpsl and libspl uses python3 as buildtool and brings it with itself
                # What a story, Mark
                enableDebuginfod = false;
            };
        };
        pkgs = import nixpkgs { inherit system; overlays = [ overlays ]; };
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
          tag = gitRev;
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
              pkgs.just
            ];

            withHoogle = false;
            shellHook = ''
                export GIT_REV=${gitRev}
            '';
          };
        }
  );
}
