{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/2e5b0b553ab52b963052a3ed76c166fd74eabbfd.tar.gz") {}
}:let

in pkgs.mkShell {

  buildInputs = with pkgs; [
    git
    jdk11_headless
    gradle
    gcc
    scala
  ];
  shellHook = with pkgs;
      ''
      export JAVA_HOME=${jdk11_headless}
      '';
}
