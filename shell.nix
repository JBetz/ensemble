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
      pkgs.ghcid
      pkgs.haskellPackages.c2hsc
      pkgs.haskellPackages.hoogle
      pkgs.haskellPackages.hlint
    ];
  }
