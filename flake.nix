{
  description = "Overengineered Hangman example";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghc = "ghc963";
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
      pkgs = import nixpkgs {
        inherit system;
        overlays = [overlays];
      };
      slim-exec = pkg: pkgs.haskell.lib.justStaticExecutables pkg;
      haskellPkgs = pkgs.haskell.packages.${ghc}.override {
        overrides = ghcSelf: ghcSuper: {
          generics-sop = pkgs.haskell.lib.doJailbreak ghcSuper.generics-sop;
          hangman = ghcSuper.callCabal2nix "hangman" ./hangman {};
          hangman-adapters =
            ghcSuper.callCabal2nix "hangman-adapters" ./hangman-adapters {};
          hangman-server =
            slim-exec
            (ghcSuper.callCabal2nix "hangman-server" ./hangman-server {});
        };
      };
      docker-image-tag = "flake-build";
      server-image = pkgs.dockerTools.buildImage {
        name = "hangman-server";
        tag = docker-image-tag;
        copyToRoot = [hangman.server];
        config = {Cmd = ["hangman-server-exe" "-p 8080"];};
      };
      hangman = {
        lib = haskellPkgs.hangman;
        adapters = haskellPkgs.hangman-adapters;
        server = haskellPkgs.hangman-server;
        server-docker = server-image;
      };
      pre-commit = pre-commit-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          stylish-haskell.enable = true;
          alejandra.enable = true;
          just-fmt = {
            enable = true;
            name = "Just fmt";
            entry = "find -iname justfile -exec ${pkgs.just}/bin/just --fmt --unstable -f {} \;";
            files = "justfile";
            pass_filenames = false;
          };
        };
      };
    in {
      packages = {
        inherit hangman;
        default = hangman.server;
      };

      devShells.default = haskellPkgs.shellFor {
        packages = p: [hangman.lib hangman.adapters hangman.server];

        buildInputs =
          (with pkgs; [just])
          ++ (with haskellPkgs; [
            cabal-install
            haskell-language-server
            hpack
          ]);

        withHoogle = false;
        shellHook = ''
          ${pre-commit.shellHook}
          export FLAKE_IMAGE_TAG=${docker-image-tag}
        '';
      };
    });
}
