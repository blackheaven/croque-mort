{
  description = "croque-mort";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    poolboy.url = "github:blackheaven/poolboy";
    poolboy.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    # flake-utils.lib.eachDefaultSystem
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskellPackages.override
          {
            overrides = hself: hsuper: {
              poolboy = jailbreakUnbreak hsuper.poolboy;
            };
          };

        nixpkgsOverlay = _final: _prev: {
          croque-mort = self.packages.${system}.croque-mort;
        };

      in
      rec {
        packages.croque-mort =
          pkgs.haskell.lib.justStaticExecutables (haskellPackages.callCabal2nix "croque-mort" ./. {
            poolboy =
              haskellPackages.callCabal2nix "poolboy" inputs.poolboy
              { };
          });

        packages.default = packages.croque-mort;

        overlays = nixpkgsOverlay;

        devShells.default =
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              ormolu
              fourmolu
            ];
            inputsFrom = [ self.packages.${system}.default.env ];
          };
      });
}
