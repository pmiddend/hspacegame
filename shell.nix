{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, apecs, base, bytestring, cabal-install
      , clock, containers, data-default-class, directory, filepath
      , hindent, hlint, lens, lens-aeson, linear, MonadRandom, random
      , sdl2, sdl2-image, sdl2-mixer, sdl2-ttf, StateVar, stdenv, text
      , time-units
      }:
      mkDerivation {
        pname = "hspacegame";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson apecs base bytestring clock containers data-default-class
          directory filepath lens lens-aeson linear MonadRandom random sdl2
          sdl2-image sdl2-mixer sdl2-ttf StateVar text time-units
        ];
        executableToolDepends = [ cabal-install hindent hlint ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
