{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz") {} }:

pkgs.mkShell {
  packages = [ pkgs.haskell-language-server (pkgs.haskellPackages.ghcWithPackages (p: [p.aeson p.hspec p.servant p.string-interpolate p.text p.time p.warp p.wai])) ];
}
