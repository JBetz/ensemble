{ mkDerivation, aeson, base, bytestring, clap, containers, deepseq
, extra, hashable, hspec, http-types, lib, mtl, optparse-generic
, portaudio, PortMidi, stm, template-haskell, text, th-abstraction
, unix, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "ensemble";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring clap containers deepseq extra hashable mtl
    optparse-generic portaudio PortMidi stm template-haskell text
    th-abstraction unix
  ];
  executableHaskellDepends = [
    aeson base bytestring extra http-types optparse-generic text wai
    wai-websockets warp websockets
  ];
  testHaskellDepends = [ base clap hspec mtl ];
  license = lib.licenses.mit;
}
