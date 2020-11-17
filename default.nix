{ mkDerivation, base, base64-bytestring, bytestring
, case-insensitive, containers, data-default, mime, mtl
, network-uri, old-locale, parsec, stdenv, text, time
}:
mkDerivation {
  pname = "iCalendar";
  version = "0.4.0.5";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bytestring case-insensitive containers
    data-default mime mtl network-uri old-locale parsec text time
  ];
  homepage = "http://github.com/chrra/iCalendar";
  description = "iCalendar data types, parser, and printer";
  license = stdenv.lib.licenses.bsd3;
}
