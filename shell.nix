with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "Raytracer";
  buildInputs =
    let haskellEnv = haskellPackages.ghc.withPackages (pkgs: with pkgs; [
	  #aeson
          stack
        ]);
    in
      [haskellEnv];
}
