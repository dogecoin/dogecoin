unspendable() {

~/unspendable/unspendable.py $*

}

. ~/alp/alp.bash

: Change this to point to the dimecash directory
export _DC_LIB=$_DC_DIR/php_library
export _DC_SRC=$_DC_DIR/dc.bash
# export _DC_WEB=~/public_html
# set in .bashrc


dc.env()  {

env | grep "_DC_"

}
dc () { dogecoin-cli $*; }
dc.get.bestblockhash () { dc getbestblockhash; }
dc.get.block () { blockhash=$1 ; dc getblock $blockhash verbose; }
dc.get.blockchaininfo () { dc getblockchaininfo; }
dc.get.blockcount () { dc getblockcount; }
dc.get.blockhash () { height=$1 ; dc getblockhash $height; }
dc.get.blockheader () { hash="$1"; dc getblockheader "$hash" verbose; }
dc.get.chaintips () { dc getchaintips; }
dc.get.difficulty () { dc getdifficulty; }
dc.get.mempool.ancestors () { txid=$1 ; dc getmempoolancestors $txid verbose; }
dc.get.mempool.descendants () { txid=$1 ; dc getmempooldescendants $txid verbose; }
dc.get.mempool.entry () { txid=$1 getmempoolentry $txid; }
dc.get.mempool.info () { dc getmempoolinfo; }
dc.get.mempool.raw () { dc getrawmempool  verbose; }
dc.gettxout () { txid=$1; n=$2; dc gettxout "$txid" $n include_mempool; }
# : gettxoutproof ["txid",...]  blockhash 
: gettxoutsetinfo
: preciousblock "blockhash"
: pruneblockchain
: verifychain checklevel nblocks
: verifytxoutproof "proof"
dc.create.raw.transaction () {
createrawtransaction 
   #        [{"txid":"id","vout":n},...] 
    #        {"address":amount,"data":"hex",...} 
     #     ( locktime ) 
}
dc.next () 
{ 
    echo $(dc.last)+1 | bc
}
dc.last () 
{ 
    cat $_DC_WEB/doge/*000.txt | tail -1 | awk '{print $1 }'
}
dc () { dogecoin-cli $*; }
dc.get.bestblockhash () { dc getbestblockhash; }
dc.get.block () { blockhash=$1 ; dc getblock $blockhash verbose; }
dc.get.blockchaininfo () { dc getblockchaininfo; }
dc.get.blockcount () { dc getblockcount; }
dc.get.blockhash () { height=$1 ; dc getblockhash $height; }
dc.get.blockheader () { hash="$1"; dc getblockheader "$hash" verbose; }
dc.get.chaintips () { dc getchaintips; }
dc.get.difficulty () { dc getdifficulty; }
dc.get.mempool.ancestors () { txid=$1 ; dc getmempoolancestors $txid verbose; }
dc.get.mempool.descendants () { txid=$1 ; dc getmempooldescendants $txid verbose; }
dc.get.mempool.entry () { txid=$1 getmempoolentry $txid; }
dc.get.mempool.info () { dc getmempoolinfo; }
dc.get.mempool.raw () { dc getrawmempool  verbose; }
dc.gettxout () { txid=$1; n=$2; dc gettxout "$txid" $n include_mempool; }
# : gettxoutproof ["txid",...]  blockhash 
: gettxoutsetinfo
: preciousblock "blockhash"
: pruneblockchain
: verifychain checklevel nblocks
: verifytxoutproof "proof"
dc.create.raw.transaction () {
dc createrawtransaction 
   #        [{"txid":"id","vout":n},...] 
    #        {"address":amount,"data":"hex",...} 
     #     ( locktime ) 
}
dc.setname () 
{ 
    SETFILE=$(dc.next)/100000*100000;
    FULLNAME=$(echo $SETFILE | bc).txt;
    echo $_DC_WEB/$1/$FULLNAME
}
dc.ips () 
{ 
    cd $_DC_WEB;
    ls -d doge | while read DIR; do
        cd $DIR;
        grep DCxiPxADDR *.txt | awk '{ print $3 }';
        cd ..;
    done | sed 's/^.*DCxiPxADDRx//g' | sed 's/y.*//g' | sed 's/z.*//g' | sed 's/x/\./g' | sed 's/o/0/g' | sort -u | grep -v "^172"
}
dc.usp9 () 
{ 
    dc.valid $(usp9 "$*")
}
dc.valid () 
{ 
    dc validateaddress $1 | grep address | cut -c 15-48
}
usp9 () 
{ 
    NAME=$(echo $* | tr 'a-z' 'A-Z');
    NAME=$(echo $NAME | sed 's/0/o/g' | sed 's/1/L/g' | sed 's/O/o/g' | sed 's/l/L/g' | sed 's/I/i/g');
    NAME=$(echo $NAME | sed 's/x/X/g' | sed 's/ /x/g' | sed 's/\-/y/g');
    LEN=$(echo $NAME | wc -c);
    PAD="";
    let MAX=27;
    while [[ $LEN -lt $MAX ]]; do
        PAD=$PAD"z";
        let LEN=LEN+1;
    done;
    CHAR="9";
    NUM=22;
    ~/unspendable/unspendable.py 9 9s$NAME$PAD $NUM
}
