{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, z3, containers, parsec }:
      mkDerivation {
        pname = "imp";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base z3 containers parsec ];
        description = "Stuff for playing around with IMP programs";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
