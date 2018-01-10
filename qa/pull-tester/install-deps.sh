#!/bin/bash

# installs test dependencies

wget https://github.com/langerhans/ltc-scrypt/archive/master.tar.gz
#echo "f00a3ebace9a9430392be950ea5f8482 master.tar.gz" | md5sum -c
tar zxf master.tar.gz
pushd ltc-scrypt-master
python3 setup.py install --user
popd
