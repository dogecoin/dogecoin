#!/bin/bash
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

oss_patch_url="https://bitcoincore.org/cfields/osslsigncode-Backports-to-1.7.1.patch"
oss_patch_hash="a8c4e9cafba922f89de0df1f2152e7be286aba73f78505169bc351a7938dd911"

oss_tar_url="https://launchpad.net/ubuntu/+archive/primary/+sourcefiles/osslsigncode/1.7.1-1/osslsigncode_1.7.1.orig.tar.gz"
oss_tar_hash="f9a8cdb38b9c309326764ebc937cba1523a3a751a7ab05df3ecc99d18ae466c9"

macos_sdk_url="https://bitcoincore.org/depends-sources/sdks/MacOSX10.11.sdk.tar.gz"
macos_sdk_hash="bec9d089ebf2e2dd59b1a811a38ec78ebd5da18cbbcd6ab39d1e59f64ac5033f"

mkdir -p inputs

function check_file() {
  echo "${1} $(basename -- $2)" | sha256sum -c > /dev/null 2>&1
  if [[ $? == 0 ]]; then
    echo "OK"
  else
    echo ""
  fi
}

function download_file() {
  wget $1 -O $(basename -- $1)
}

function process_file() {
  if [[ ! -f $(basename -- $1) || ! $(check_file $2 $1) ]]; then
    download_file "$1"
    [[ $(check_file "$2" "$1") ]] || { echo "Signature for $(basename -- $1) don't match"; exit 1; }
  fi

}

pushd inputs

process_file $oss_patch_url $oss_patch_hash
process_file $oss_tar_url $oss_tar_hash
process_file $macos_sdk_url $macos_sdk_hash

popd
