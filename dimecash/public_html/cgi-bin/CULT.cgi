#!/bin/bash
echo "Content-type: text/html"
echo

echo "<h1>"
echo 'DCULToFTHEDoLPHiNZZZZZZZZZZZZLkeq5'
echo "</h1>"
echo "<h2> (Cult of The Dolphin) "
echo "</h2>"
echo "<pre>"
curl https://dogechain.info/api/v1/unspent/DCULToFTHEDoLPHiNZZZZZZZZZZZZLkeq5 \
| egrep "tx_hash|value" | xargs -n6
echo "</pre>"
