{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", profiling ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then if profiling
                            then pkgs.profiledHaskellPackages
                            else pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./default.nix { inherit profiling; };

in

  if pkgs.lib.inNixShell then drv.env else drv
