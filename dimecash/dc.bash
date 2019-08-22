unspendable() {

~/unspendable/unspendable.py $*

}
# User specific aliases and functions
alias php=/usr/local/php70/bin/php

. ~/alp/alp.bash

: Change this to point to the dimecash directory
export _DC_DIR=~/dimecash
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
dc.last () 
{ 
    ls $_DC_WEB/doge/last | tail -1
}
dc.next () 
{ 
    echo $(dc.last)+1 | bc
}
