{ compiler ? "default" }:
let
  pkgs = import <nixpkgs> { inherit config; };
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          site = super.callCabal2nix "super-app-docs" (gitIgnore [./.gitignore] ./.) {};
        };
      };
    };
  };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

in {
  inherit pkgs;
  site = haskellPackages.site;
}
