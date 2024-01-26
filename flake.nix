{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc8107;

      in {
        formatter = pkgs.nixfmt;

        devShells = let
          ## The minimal dependency set to build the project with `cabal`.
          buildInputs = ([ hpkgs.ghc ]) ++ (with pkgs; [
            cabal-install
            zlib
          ]);

        in {
          ci = pkgs.mkShell {
            inherit buildInputs;
          };

          default = pkgs.mkShell {
            ## NOTE: `pkgs.ormolu` must appear before `hpkgs.haskell-language-server`
            ## in the `buildInputs`, so as to take precedence. This ensures that the
            ## version of Ormolu available in the path is that of nixpkgs and not the
            ## one pinned by HLS.
            buildInputs = buildInputs ++ (with pkgs; [ ormolu hpack hlint ]);
          };
        };
      });
}