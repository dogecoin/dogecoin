#!/bin/bash
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

$sudo pacman -Suy --noconfirm --overwrite "*" docker
$sudo systemctl start docker
$sudo systemctl enable docker

$sudo usermod -aG docker $USER