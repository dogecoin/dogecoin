#!/bin/bash

# installs test dependencies

curl -L https://github.com/langerhans/ltc-scrypt/archive/master.tar.gz --output master.tar.gz
echo "ade3cdf498927990b6d153d74f0da104114e838584be5a81bef8972accd03341 master.tar.gz" | sha256sum -c
tar zxf master.tar.gz
pushd ltc-scrypt-master
python3 setup.py install --user
popd
