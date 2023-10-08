#!/usr/bin/env bash
#
# Copyright (c) 2019-2020 The Bitcoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

export LC_ALL=C.UTF-8

export HOST=x86_64-apple-darwin22.4.0
export PIP_PACKAGES="zmq"
export GOAL="install"
# export BDB_PREFIX="${BASE_ROOT_DIR}/db5"
export HOMEBREW_DIR="/opt/homebrew/Cellar"
export BDB_PREFIX="${HOMEBREW_DIR}/berkeley-db/18.1.40_1"
export CPPFLAGS="-I${HOMEBREW_DIR}/libevent/2.1.12/include"
export LDFLAGS="-L${HOMEBREW_DIR}/boost/1.81.0_1/lib"
export BOOST_DIR="${HOMEBREW_DIR}/boost/1.81.0_1"
export BITCOIN_CONFIG="--with-gui --enable-reduce-exports --with-boost='$BOOST_DIR' --with-boost-process BDB_LIBS='-L${BDB_PREFIX}/lib -ldb_cxx' BDB_CFLAGS='-I${BDB_PREFIX}/include'"
export CI_OS_NAME="macos"
export NO_DEPENDS=1
export OSX_SDK=""
export CCACHE_SIZE=300M


# if set to 'true', will get:
# travis_fold:start:security-tests
# ld: the target architecture doesn't support executable stacks
export RUN_SECURITY_TESTS="false"

# Compiler for Mac native warns on C99 in dependencies.
export NO_WERROR=1
