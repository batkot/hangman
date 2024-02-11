{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPkgs = pkgs.haskellPackages.extend (self: super: {
            hangman = self.callCabal2nix "hangman" ./hangman {};
            hangman-adapters = self.callCabal2nix "hangman-adapters" ./hangman-adapters {};
            hangman-server = self.callCabal2nix "hangman-server" ./hangman-server {};
        });
        hangman = {
            lib = haskellPkgs.hangman;
            adapters = haskellPkgs.hangman-adapters;
            server = haskellPkgs.hangman-server;
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
