#!/bin/bash

# installs test dependencies
file=v1.0.1.tar.gz
curl -L -O https://github.com/dogecoin/ltc-scrypt/archive/refs/tags/$file
echo "e866ade37fb27439ae0ca32f1ee4ad32be428c1fdac9bcc988b36c68648ff0de  $file" | sha256sum -c
python3 -m pip install $file --user
rm -rf $file
