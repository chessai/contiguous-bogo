{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  contiguous = self.callCabal2nix "contiguous" (pkgs.fetchFromGitHub {
    owner = "andrewthad";
    repo = "contiguous";
    rev = "bc09696e47ca6982f9732230d5a1330d5e8fcbc4"; 
    sha256 = "1y4f01i1113da0ypsr8hgi4hqv4m2iyiq8c8ds3crfpkn4dhs0v0";
  }) {}; 
 
  contiguous-bogo = (
    with rec {
      contiguous-bogoSource = pkgs.lib.cleanSource ../.;
      contiguous-bogoBasic  = self.callCabal2nix "contiguous-bogo" contiguous-bogoSource { };
    };
    overrideCabal contiguous-bogoBasic (old: {
    })
  );
}
