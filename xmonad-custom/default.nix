{ pkgs
}:
with pkgs; with stdenv; with haskellPackages;
pkgs.haskellPackages.mkDerivation {
  pname = "xmonad-custom";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring xmonad xmonad-contrib
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring xmonad xmonad-contrib
  ];
  testHaskellDepends = [
    aeson base bytestring xmonad xmonad-contrib
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/githubuser/xmonad-custom#readme";
  license = stdenv.lib.licenses.bsd3;
}
