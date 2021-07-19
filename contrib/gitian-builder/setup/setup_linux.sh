#!/bin/bash
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

if command -v sudo &> /dev/null
then
  export sudo="sudo "
fi

$sudo $package_manager_update