#!/bin/bash
echo "Content-type: text/html"
echo

eval $(echo $QUERY_STRING | sed 's/&/ /g' )
ANCHOR=$(./unspendable.py D "DCx"$name 30)

echo "redirect to:"

echo "<a href=https://dogechain.info/address/$ANCHOR> $ANCHOR </a>"
