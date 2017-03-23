{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let nixpkgs' = nixpkgs;
in with nixpkgs'.pkgs;
let hp = haskell.packages.${compiler}.override{
    overrides = self: super: {
      irc = self.callPackage ./irc.nix {};
      };};
     getHaskellDeps = ps: path:
        let f = import path;
            gatherDeps = {buildDepends ? [], libraryHaskellDepends ? [], executableHaskellDepends ? [], ...}:
               libraryHaskellDepends ++ executableHaskellDepends;
            x = f (builtins.intersectAttrs (builtins.functionArgs f) ps // {stdenv = stdenv; mkDerivation = gatherDeps;});
        in x;
ghc = hp.ghcWithPackages (ps: with ps; stdenv.lib.lists.subtractLists
[irc]
([

  ]  ++ getHaskellDeps ps ./irc.nix));
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
