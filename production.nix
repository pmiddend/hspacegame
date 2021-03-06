{ mkDerivation, aeson, apecs, base, bytestring, cabal-install
, clock, containers, data-default-class, directory, filepath
, hindent, hlint, lens, lens-aeson, lifted-base, linear
, MonadRandom, random, sdl2, sdl2-image, sdl2-mixer, sdl2-ttf
, StateVar, stdenv, text, time-units
}:
mkDerivation {
  pname = "hspacegame";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson apecs base bytestring clock containers data-default-class
    directory filepath lens lens-aeson lifted-base linear MonadRandom
    random sdl2 sdl2-image sdl2-mixer sdl2-ttf StateVar text time-units
  ];
  executableToolDepends = [ cabal-install hindent hlint ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
