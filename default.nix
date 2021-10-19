{ compiler ? "ghc8104" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "advent-of-haskell" =
        hself.callCabal2nix
          "advent-of-haskell"
          (gitignore ./.)
          { };
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."advent-of-haskell"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."advent-of-haskell");

  docker = pkgs.dockerTools.buildImage {
    name = "advent-of-haskell";
    config.Cmd = [ "${exe}/bin/advent-of-haskell" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "advent-of-haskell" = myHaskellPackages."advent-of-haskell";
}
