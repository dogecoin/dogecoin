{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  nativeBuildInputs = [
    pkg-config
    autoreconfHook
    openssl
    db5
    util-linux
    boost
    zlib
    libevent
    miniupnpc
    qt4
    protobuf
    qrencode
  ];
}
