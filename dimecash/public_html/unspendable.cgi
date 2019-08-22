#!/bin/bash
echo "Content-type: text/html"
echo

eval $(echo $QUERY_STRING | sed 's/&/ /g' )
./unspendable.py $first "DCx"$name $seed
