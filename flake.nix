{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays =
          [ haskellNix.overlay
              (final: prev: {
                hixProject =
                  final.haskell-nix.project {
                    src = ./.;
                    compiler-nix-name = "ghc8107";
                    evalSystem = "aarch64-darwin";
                  };
                }
              )
          ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // rec
           { legacyPackages = pkgs;
              packages =  
                { lib = flake.packages."stalk:lib:stalk";
                  all = pkgs.symlinkJoin {
                    name = "all";
                    paths = with packages;
                      [ lib
                      ];
                  };
                  default = packages.all;
                };
             devShells =
               { default =
                  pkgs.hixProject.shellFor {
                    tools = {
                      cabal = {};
                      # haskell-language-server = "2.4.0.0";
                    };
                    buildInputs = with pkgs; [
                     # haskellPackages.ormolu_0_5_2_0
                     # haskellPackages.cabal-fmt
                    ];

                  };
               };
           }
      );
  nixConfig = {
    allow-import-from-derivation = true;
  };
}
