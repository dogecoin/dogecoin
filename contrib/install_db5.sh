#!/bin/sh
# Copyright (c) 2017-2019 The Bitcoin Core developers
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

# Install libdb5.3 (Berkeley DB).

export LC_ALL=C
set -e

if [ -z "${1}" ]; then
  echo "Usage: $0 <base-dir> [<extra-bdb-configure-flag> ...]"
  echo
  echo "Must specify a single argument: the directory in which db5 will be built."
  echo "This is probably \`pwd\` if you're at the root of the bitcoin repository."
  exit 1
fi

expand_path() {
  cd "${1}" && pwd -P
}

BDB_PREFIX="$(expand_path ${1})/db5"; shift;
BDB_VERSION='db-5.3.28.NC'
BDB_HASH='76a25560d9e52a198d37a31440fd07632b5f1f8f9f2b6d5438f4bc3e7c9013ef'
BDB_URL="https://download.oracle.com/berkeley-db/${BDB_VERSION}.tar.gz"

check_exists() {
  command -v "$1" >/dev/null
}

sha256_check() {
  # Args: <sha256_hash> <filename>
  #
  if check_exists sha256sum; then
    echo "${1}  ${2}" | sha256sum -c
  elif check_exists sha256; then
    if [ "$(uname)" = "FreeBSD" ]; then
      sha256 -c "${1}" "${2}"
    else
      echo "${1}  ${2}" | sha256 -c
    fi
  else
    echo "${1}  ${2}" | shasum -a 256 -c
  fi
}

http_get() {
  # Args: <url> <filename> <sha256_hash>
  #
  # It's acceptable that we don't require SSL here because we manually verify
  # content hashes below.
  #
  if [ -f "${2}" ]; then
    echo "File ${2} already exists; not downloading again"
  elif check_exists curl; then
    curl --insecure --retry 5 "${1}" -o "${2}"
  else
    wget --no-check-certificate "${1}" -O "${2}"
  fi

  sha256_check "${3}" "${2}"
}

mkdir -p "${BDB_PREFIX}"
http_get "${BDB_URL}" "${BDB_VERSION}.tar.gz" "${BDB_HASH}"
tar -xzvf ${BDB_VERSION}.tar.gz -C "$BDB_PREFIX"
cd "${BDB_PREFIX}/${BDB_VERSION}/"

cd build_unix/

"${BDB_PREFIX}/${BDB_VERSION}/dist/configure" \
  --enable-cxx --disable-shared --disable-replication --with-pic --prefix="${BDB_PREFIX}" \
  "${@}"

make install

echo
echo "db5 build complete."
echo
# shellcheck disable=SC2016
echo 'When compiling bitcoind, run `./configure` in the following way:'
echo
echo "  export BDB_PREFIX='${BDB_PREFIX}'"
# shellcheck disable=SC2016
echo '  ./configure BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx-5.3" BDB_CFLAGS="-I${BDB_PREFIX}/include" ...'
