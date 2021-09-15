{ mkDerivation, base, binary, bytestring, directory, lib, network
, optparse-applicative, storable-tuple, text, time
, time-locale-compat
}:
mkDerivation {
  pname = "Utmp";
  version = "0.1.0.0";
  src = lib.cleanSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring network storable-tuple text time
    time-locale-compat
  ];
  executableHaskellDepends = [
    base binary bytestring directory optparse-applicative time
    time-locale-compat
  ];
  description = "A library for reading and writing Linux utmp files";
  license = lib.licenses.mit;
}
