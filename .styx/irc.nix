{ mkDerivation, aeson, base, bytestring, mtl, network-simple, pipes
, servant-server, stdenv, stm, text, transformers, wai
, wai-websockets
}:
mkDerivation {
  pname = "irc";
  version = "0.1.0.0";
  src = /home/masse/programming/irc;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring mtl network-simple pipes servant-server stm
    text transformers wai wai-websockets
  ];
  license = stdenv.lib.licenses.bsd3;
}
