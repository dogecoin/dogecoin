#!/bin/bash

. ~/dogecoin/dimecash/dc.bash

Process () 
{ 
    BLOCK=$1;
    TEMP="$BLOCK.json"
    TEMP0="$BLOCK.0.json"
    TEMP1="$BLOCK.1.json"
    export _DC_LAST=$(dc.get.blockhash $BLOCK);
    dc getblock $_DC_LAST > $TEMP
    TME=$(cat $TEMP | jq .time)
  #   TME=$(dc getblock $_DC_LAST | jq .time)
    cat $TEMP | jq .tx[] | xargs -n1 | while read TX; do
        dc gettxout $TX 0 > $TEMP0
        dc gettxout $TX 1 > $TEMP1
        cat $TEMP0 | jq .scriptPubKey.addresses;
        cat $TEMP0 | jq .value;
        cat $TEMP1 | jq .scriptPubKey.addresses;
        cat $TEMP1 | jq .value;
    done | xargs -n4 | grep  "\[" | while read A B C D; do
        echo $BLOCK $TME $B $D;
    done
    rm $TEMP $TEMP0 $TEMP1
}

while true
do
let FINAL=$(dc.get.blockcount )
let BLOCK=$(dc.next )
while [[ $BLOCK -le $FINAL ]] 
do
TARGET=$(dc.setname doge )
Process $BLOCK >> $TARGET
let BLOCK=$BLOCK+1
done
sleep 20
done
