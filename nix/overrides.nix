{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  contiguous-bogo = (
    with rec {
      contiguous-bogoSource = pkgs.lib.cleanSource ../.;
      contiguous-bogoBasic  = self.callCabal2nix "contiguous-bogo" contiguous-bogoSource { };
    };
    overrideCabal contiguous-bogoBasic (old: {
    })
  );
}
