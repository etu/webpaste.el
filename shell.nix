with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    # Tooling
    (emacsWithPackages (epkgs: (with epkgs; [
      cask
      request
      buttercup
      undercover
    ])))
    cask
    gnumake
  ];
}
