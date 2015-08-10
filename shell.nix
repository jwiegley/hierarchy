{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, doctest, exceptions
      , filepath, free, hspec, hspec-expectations, mmorph, monad-control
      , mtl, pipes, semigroups, stdenv, transformers, transformers-base
      , transformers-compat
      }:
      mkDerivation {
        pname = "hierarchy";
        version = "0.3.0";
        src = ./.;
        libraryHaskellDepends = [
          base exceptions free mmorph monad-control mtl pipes semigroups
          transformers transformers-base transformers-compat
        ];
        testHaskellDepends = [
          base directory doctest filepath hspec hspec-expectations mtl pipes
          semigroups transformers
        ];
        homepage = "https://github.com/jwiegley/hierarchy";
        description = "Pipes-based library for predicated traversal of generated trees";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
