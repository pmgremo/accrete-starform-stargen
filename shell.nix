{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/21de2b973f9fee595a7a1ac4693efff791245c34.tar.gz") {}
}:let

in pkgs.mkShell {

  buildInputs = with pkgs; [
    git
    jdk17_headless
    gradle
    gcc
    dotty
  ];
}
