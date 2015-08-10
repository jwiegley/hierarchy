{ mkDerivation, base, directory, exceptions, free, mmorph
, monad-control, mtl, pipes, semigroups, stdenv, transformers
, transformers-base, unix, doctest, hspec, hspec-expectations
, transformers-compat, attoparsec, bytestring, text, time, filepath
, posix-paths, unix-compat, regex-posix, pipes-safe
}:
mkDerivation {
  pname = "hierarchy";
  version = "0.3.1";
  src = ./.;
  libraryHaskellDepends = [
    base directory exceptions free mmorph monad-control mtl pipes
    semigroups transformers transformers-base transformers-compat unix
    attoparsec bytestring text time filepath posix-paths unix-compat
    regex-posix pipes-safe
  ];
  testHaskellDepends = [ doctest hspec hspec-expectations ];
  homepage = "https://github.com/jwiegley/hierarchy";
  description = "Provide a TreeT type for generating trees, as ListT does for lists";
  license = stdenv.lib.licenses.bsd3;
}
