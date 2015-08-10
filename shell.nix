{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, doctest, exceptions
      , filepath, free, hspec, hspec-expectations, mmorph, monad-control
      , mtl, pipes, semigroups, stdenv, transformers, transformers-base
      , transformers-compat, unix
      }:
      mkDerivation {
        pname = "hierarchy";
        version = "0.1.1";
        src = ./.;
        libraryHaskellDepends = [
          base directory exceptions free mmorph monad-control mtl pipes
          semigroups transformers transformers-base transformers-compat unix
        ];
        testHaskellDepends = [
          base directory doctest filepath hspec hspec-expectations mtl pipes
          semigroups transformers unix
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
