let
  pkgs = import <nixpkgs> {};
  rPackages = with pkgs.rPackages; [ devtools ggplot2 dplyr languageserver gdata ];
  myRstudioWrapper = pkgs.rstudioWrapper.override {
    packages = rPackages;
  };
  myRWrapper = pkgs.rWrapper.override {
    packages =rPackages;
  };
in
pkgs.stdenv.mkDerivation {
  name = "sat-solving";
  buildInputs = with pkgs; [
    gmp
    zlib
    ncurses
    minisat

    haskellPackages.cabal-install
    myRstudioWrapper
    myRWrapper
  ];
  src = null;
  shellHook = with pkgs; ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
