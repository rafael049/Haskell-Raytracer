with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "Raytracer";
  buildInputs =
    let haskellEnv = haskell.packages.ghc925.ghcWithPackages (pkgs: with pkgs; [
          stack
          bytestring
          #haskell-language-server
        ]);
    in
      [haskellEnv];
}
