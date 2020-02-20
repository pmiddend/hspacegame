{ mkDerivation, base, bytestring, cabal-install, containers
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
}
