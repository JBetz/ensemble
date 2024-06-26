let
  pkgs = import ./pkgs.nix;
  ensemble = pkgs.haskell.lib.overrideCabal (pkgs.customHaskellPackages.callCabal2nix "ensemble" ./. { }) {
    src = if pkgs.lib.inNixShell then null else ./.;
  };
in
  pkgs.mkShell {
    inputsFrom = [
      ensemble.env
    ];
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
      pkgs.haskell-language-server
      pkgs.haskellPackages.ghc
      pkgs.haskellPackages.hoogle
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.ormolu
      pkgs.cabal2nix
    ];
    shellHook = ''
      export LD_LIBRARY_PATH="${pkgs.fluidsynth}/lib"
    '';
  }
