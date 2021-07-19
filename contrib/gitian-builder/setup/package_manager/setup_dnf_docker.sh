#!/bin/bash
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

. /etc/os-release
os=`echo $NAME | awk '{print tolower($0)}'`

case $os in
  'centos'*)
    os='centos'
    ;;
  'fedora'*)
    os='fedora'
    ;;
esac

echo $os

$sudo dnf -y install dnf-plugins-core
$sudo dnf config-manager \
  --add-repo \
  https://download.docker.com/linux/${os}/docker-ce.repo
$sudo dnf -y install docker-ce docker-ce-cli containerd.io

$sudo systemctl start docker
$sudo systemctl enable docker