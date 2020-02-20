{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cabal-install, containers
      , directory, filepath, hindent, hlint, lens, linear, sdl2
      , sdl2-image, sdl2-mixer, sdl2-ttf, StateVar, stdenv, text
      }:
      mkDerivation {
        pname = "hspacegame";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring containers directory filepath lens linear sdl2
          sdl2-image sdl2-mixer sdl2-ttf StateVar text
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
