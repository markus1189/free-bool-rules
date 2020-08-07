{ ghcVersion ? "ghc883" }:
let
  hostNixpkgs = import <nixpkgs>;
  overlay = self: super: {
    myHaskellPackages = super.haskell.packages.${ghcVersion};
  };
  pkgs = hostNixpkgs {
    overlays = [ overlay ];
  };
  src = ./.;
  drv = pkgs.myHaskellPackages.callCabal2nix "free-bool-rules" src {};
in
  drv
