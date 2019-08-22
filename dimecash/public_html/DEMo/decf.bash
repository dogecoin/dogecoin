#!/bin/bash

SECRETS="/home/dime/secrets";
NAME=$(cat myfile.txt | head -1 | sed 's/ .*//g')
cat myfile.txt | sed 's///g' > myfile.out
. ./myfile.out
rm myfile.*
cp template.txt myfile.txt
. /home/dime/alp/alp.bash
cd $SECRETS
a.f $NAME
echo "# " $(a.fs $NAME)
