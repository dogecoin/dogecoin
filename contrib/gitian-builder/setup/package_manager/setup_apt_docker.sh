#!/bin/bash
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

# FIX TZDATA INTERACTIVE
echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

$sudo apt update
$sudo apt install -y ruby git build-essential apt-transport-https ca-certificates \
     curl gnupg-agent software-properties-common

. /etc/os-release

os=`echo $ID | awk '{print tolower($0)}'`
release=`echo $VERSION_CODENAME | awk '{print tolower($0)}'`

curl -fsSL https://download.docker.com/linux/$os/gpg | $sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo \
  "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/$os \
  $release stable" | $sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

$sudo apt update

$sudo apt install -y docker-ce

$sudo usermod -aG docker `whoami`
#newgrp docker
$sudo systemctl enable docker
$sudo systemctl start docker