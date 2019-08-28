#!/bin/bash

git clone https://github.com/johnrigler/dogecoin.git
cd dogecoin
./autogen.sh
./configure --disable-wallet 
make
sudo make install
echo dogecoind | at now
