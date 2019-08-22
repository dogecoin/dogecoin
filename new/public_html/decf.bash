#!/bin/bash

FILE=$1
SECRETS="/home/dime/secrets";
NAME=$(cat $FILE | head -1 | sed 's/ .*//g')
cat $FILE | sed 's///g' > myfile.out
. ./myfile.out
rm myfile.*
mv $1 public
. /home/dime/alp/alp.bash
cd $SECRETS
a.f $NAME
echo "# " $(a.fs $NAME)
