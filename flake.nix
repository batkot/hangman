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
    # TODO: Update flake
    web-view = {
      url = "github:seanhess/web-view";
      flake = false;
    };
    hyperbole = {
      url = "github:seanhess/hyperbole?rev=68d6c65e4f7740c1d1ce78ca4758282b1393b6e5";
      flake = false;
    };
    effectful = {
      url = "github:haskell-effectful/effectful?rev=6e61d382240037df3234c6d68bccfe461bd4d8d0";
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
    effectful,
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
          web-view = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (ghcSuper.callCabal2nix "web-view" web-view.outPath {}));
          hyperbole = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (ghcSuper.callCabal2nix "hyperbole" hyperbole.outPath {}));
          effectful-core = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (ghcSuper.callCabal2nix "effectful-core" (effectful.outPath + "/effectful-core") {}));
          effectful = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (ghcSuper.callCabal2nix "effectful" (effectful.outPath + "/effectful") {}));
          generics-sop = pkgs.haskell.lib.doJailbreak ghcSuper.generics-sop;
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
