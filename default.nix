with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "hex2sh";
  buildInputs = [
    ( haskellPackages.ghcWithPackages (ghcpkgs: with ghcpkgs;
        [ trifecta ] ) )
  ];
}
