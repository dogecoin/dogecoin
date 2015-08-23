#!/bin/bash

# installs test dependencies

wget https://pypi.python.org/packages/source/l/ltc_scrypt/ltc_scrypt-1.0.tar.gz
#echo "7d019c3c98f16eb466a272e518ffb014 ltc_scrypt-1.0.tar.gz" | md5sum -c
tar zxf ltc_scrypt-1.0.tar.gz
pushd ltc_scrypt-1.0
python setup.py install --user
popd
