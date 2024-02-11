{
  description = "Overengineered Hangman example";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        slim = pkg: pkg.overrideAttrs (oldAttrs: {
          enableSharedExecutables = false;
          postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
        });
        haskellPkgs = pkgs.haskellPackages.extend (self: super: {
            hangman = self.callCabal2nix "hangman" ./hangman {};
            hangman-adapters = self.callCabal2nix "hangman-adapters" ./hangman-adapters {};
            hangman-server = slim (self.callCabal2nix "hangman-server" ./hangman-server {});
        });
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

          devShells.default = pkgs.haskellPackages.shellFor {
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
