{
  description = "Overengineered Hangman example";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=4a817d2083d6cd7068dc55511fbf90f84653b301";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    # TODO: Update flake
    web-view = {
      url = "github:seanhess/web-view";
      flake = false;
    };
    hyperbole = {
      url = "github:seanhess/hyperbole?rev=68d6c65e4f7740c1d1ce78ca4758282b1393b6e5";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
    web-view,
    hyperbole,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghc = "ghc982";
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
        config.allowBroken = true;
        # overlays = [overlays];
      };
      slim-exec = pkg: pkgs.haskell.lib.justStaticExecutables pkg;
      slim = pkg:
        pkg.overrideAttrs (oldAttrs: {
          enableSharedExecutables = false;
          postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
        });
      haskellPkgs = pkgs.haskell.packages.${ghc}.override {
        overrides = ghcSelf: ghcSuper: {
          fast-logger = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak ghcSuper.fast-logger);
          websockets = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak ghcSuper.websockets);
          scotty = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak ghcSuper.scotty);
          web-view = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (ghcSuper.callCabal2nix "web-view" web-view.outPath {}));
          hyperbole = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (ghcSuper.callCabal2nix "hyperbole" hyperbole.outPath {}));
          hangman = ghcSuper.callCabal2nix "hangman" ./hangman {};
          hangman-adapters =
            ghcSuper.callCabal2nix "hangman-adapters" ./hangman-adapters {};
          hangman-server =
            slim
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
