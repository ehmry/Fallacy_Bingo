{ pkgs ? import <nixpkgs> { } }:
pkgs.myEnvFun {
  name = "fallacybingo";
  buildInputs = with pkgs; [
    R rPackages.gridExtra
    gnumake
  ];
  extraCmds = "export R_LIBS_USER=\"$HOME/.nix-profile/library\"";
}
