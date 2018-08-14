with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    # Tooling
    emacs
    emacsPackages.cask
    gnumake
  ];
}
