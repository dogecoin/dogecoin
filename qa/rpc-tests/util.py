# Copyright (c) 2014 The Bitcoin Core developers
# Distributed under the MIT/X11 software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
#
# Helpful routines for regression testing
#

# Add python-bitcoinrpc to module search path:
import os
import sys
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "python-bitcoinrpc"))

from decimal import Decimal, ROUND_DOWN
import json
import shutil
import subprocess
import time

from bitcoinrpc.authproxy import AuthServiceProxy, JSONRPCException
from util import *

START_P2P_PORT=11000
START_RPC_PORT=11100

def check_json_precision():
    """Make sure json library being used does not lose precision converting BTC values"""
    n = Decimal("20000000.00000003")
    satoshis = int(json.loads(json.dumps(float(n)))*1.0e8)
    if satoshis != 2000000000000003:
        raise RuntimeError("JSON encode/decode loses precision")

def sync_blocks(rpc_connections):
    """
    Wait until everybody has the same block count
    """
    while True:
        counts = [ x.getblockcount() for x in rpc_connections ]
        if counts == [ counts[0] ]*len(counts):
            break
        time.sleep(1)

def sync_mempools(rpc_connections):
    """
    Wait until everybody has the same transactions in their memory
    pools
    """
    while True:
        pool = set(rpc_connections[0].getrawmempool())
        num_match = 1
        for i in range(1, len(rpc_connections)):
            if set(rpc_connections[i].getrawmempool()) == pool:
                num_match = num_match+1
        if num_match == len(rpc_connections):
            break
        time.sleep(1)
        

bitcoind_processes = []

def initialize_chain(test_dir):
    """
    Create (or copy from cache) a 200-block-long chain and
    4 wallets.
    bitcoind and bitcoin-cli must be in search path.
    """

    if not os.path.isdir(os.path.join("cache", "node0")):
        devnull = open("/dev/null", "w+")
        # Create cache directories, run bitcoinds:
        for i in range(4):
            datadir = os.path.join("cache", "node"+str(i))
            os.makedirs(datadir)
            with open(os.path.join(datadir, "bitcoin.conf"), 'w') as f:
                f.write("regtest=1\n");
                f.write("rpcuser=rt\n");
                f.write("rpcpassword=rt\n");
                f.write("port="+str(START_P2P_PORT+i)+"\n");
                f.write("rpcport="+str(START_RPC_PORT+i)+"\n");
            args = [ "bitcoind", "-keypool=1", "-datadir="+datadir ]
            if i > 0:
                args.append("-connect=127.0.0.1:"+str(START_P2P_PORT))
            bitcoind_processes.append(subprocess.Popen(args))
            subprocess.check_call([ "bitcoin-cli", "-datadir="+datadir,
                                    "-rpcwait", "getblockcount"], stdout=devnull)
        devnull.close()
        rpcs = []
        for i in range(4):
            try:
                url = "http://rt:rt@127.0.0.1:%d"%(START_RPC_PORT+i,)
                rpcs.append(AuthServiceProxy(url))
            except:
                sys.stderr.write("Error connecting to "+url+"\n")
                sys.exit(1)

        # Create a 200-block-long chain; each of the 4 nodes
        # gets 25 mature blocks and 25 immature.
        for i in range(4):
            rpcs[i].setgenerate(True, 25)
            sync_blocks(rpcs)
        for i in range(4):
            rpcs[i].setgenerate(True, 25)
            sync_blocks(rpcs)

        # Shut them down, and remove debug.logs:
        stop_nodes(rpcs)
        wait_bitcoinds()
        for i in range(4):
            os.remove(debug_log("cache", i))

    for i in range(4):
        from_dir = os.path.join("cache", "node"+str(i))
        to_dir = os.path.join(test_dir,  "node"+str(i))
        shutil.copytree(from_dir, to_dir)

def start_nodes(num_nodes, dir):
    # Start bitcoinds, and wait for RPC interface to be up and running:
    devnull = open("/dev/null", "w+")
    for i in range(num_nodes):
        datadir = os.path.join(dir, "node"+str(i))
        args = [ "bitcoind", "-datadir="+datadir ]
        bitcoind_processes.append(subprocess.Popen(args))
        subprocess.check_call([ "bitcoin-cli", "-datadir="+datadir,
                                  "-rpcwait", "getblockcount"], stdout=devnull)
    devnull.close()
    # Create&return JSON-RPC connections
    rpc_connections = []
    for i in range(num_nodes):
        url = "http://rt:rt@127.0.0.1:%d"%(START_RPC_PORT+i,)
        rpc_connections.append(AuthServiceProxy(url))
    return rpc_connections

def debug_log(dir, n_node):
    return os.path.join(dir, "node"+str(n_node), "regtest", "debug.log")

def stop_nodes(nodes):
    for i in range(len(nodes)):
        nodes[i].stop()
    del nodes[:] # Emptying array closes connections as a side effect

def wait_bitcoinds():
    # Wait for all bitcoinds to cleanly exit
    for bitcoind in bitcoind_processes:
        bitcoind.wait()
    del bitcoind_processes[:]

def connect_nodes(from_connection, node_num):
    ip_port = "127.0.0.1:"+str(START_P2P_PORT+node_num)
    from_connection.addnode(ip_port, "onetry")
    # poll until version handshake complete to avoid race conditions
    # with transaction relaying
    while any(peer['version'] == 0 for peer in from_connection.getpeerinfo()):
        time.sleep(0.1)

def find_output(node, txid, amount):
    """
    Return index to output of txid with value amount
    Raises exception if there is none.
    """
    txdata = node.getrawtransaction(txid, 1)
    for i in range(len(txdata["vout"])):
        if txdata["vout"][i]["value"] == amount:
            return i
    raise RuntimeError("find_output txid %s : %s not found"%(txid,str(amount)))

def gather_inputs(from_node, amount_needed):
    """
    Return a random set of unspent txouts that are enough to pay amount_needed
    """
    utxo = from_node.listunspent(1)
    random.shuffle(utxo)
    inputs = []
    total_in = Decimal("0.00000000")
    while total_in < amount_needed and len(utxo) > 0:
        t = utxo.pop()
        total_in += t["amount"]
        inputs.append({ "txid" : t["txid"], "vout" : t["vout"], "address" : t["address"] } )
    if total_in < amount_needed:
        raise RuntimeError("Insufficient funds: need %d, have %d"%(amount+fee*2, total_in))
    return (total_in, inputs)

def make_change(from_node, amount_in, amount_out, fee):
    """
    Create change output(s), return them
    """
    outputs = {}
    amount = amount_out+fee
    change = amount_in - amount
    if change > amount*2:
        # Create an extra change output to break up big inputs
        change_address = from_node.getnewaddress()
        # Split change in two, being careful of rounding:
        outputs[change_address] = Decimal(change/2).quantize(Decimal('0.00000001'), rounding=ROUND_DOWN)
        change = amount_in - amount - outputs[change_address]
    if change > 0:
        outputs[from_node.getnewaddress()] = change
    return outputs

def send_zeropri_transaction(from_node, to_node, amount, fee):
    """
    Create&broadcast a zero-priority transaction.
    Returns (txid, hex-encoded-txdata)
    Ensures transaction is zero-priority by first creating a send-to-self,
    then using it's output
    """

    # Create a send-to-self with confirmed inputs:
    self_address = from_node.getnewaddress()
    (total_in, inputs) = gather_inputs(from_node, amount+fee*2)
    outputs = make_change(from_node, total_in, amount+fee, fee)
    outputs[self_address] = float(amount+fee)

    self_rawtx = from_node.createrawtransaction(inputs, outputs)
    self_signresult = from_node.signrawtransaction(self_rawtx)
    self_txid = from_node.sendrawtransaction(self_signresult["hex"], True)

    vout = find_output(from_node, self_txid, amount+fee)
    # Now immediately spend the output to create a 1-input, 1-output
    # zero-priority transaction:
    inputs = [ { "txid" : self_txid, "vout" : vout } ]
    outputs = { to_node.getnewaddress() : float(amount) }

    rawtx = from_node.createrawtransaction(inputs, outputs)
    signresult = from_node.signrawtransaction(rawtx)
    txid = from_node.sendrawtransaction(signresult["hex"], True)

    return (txid, signresult["hex"])

def random_zeropri_transaction(nodes, amount, min_fee, fee_increment, fee_variants):
    """
    Create a random zero-priority transaction.
    Returns (txid, hex-encoded-transaction-data, fee)
    """
    from_node = random.choice(nodes)
    to_node = random.choice(nodes)
    fee = min_fee + fee_increment*random.randint(0,fee_variants)
    (txid, txhex) = send_zeropri_transaction(from_node, to_node, amount, fee)
    return (txid, txhex, fee)

def random_transaction(nodes, amount, min_fee, fee_increment, fee_variants):
    """
    Create a random transaction.
    Returns (txid, hex-encoded-transaction-data, fee)
    """
    from_node = random.choice(nodes)
    to_node = random.choice(nodes)
    fee = min_fee + fee_increment*random.randint(0,fee_variants)

    (total_in, inputs) = gather_inputs(from_node, amount+fee)
    outputs = make_change(from_node, total_in, amount, fee)
    outputs[to_node.getnewaddress()] = float(amount)

    rawtx = from_node.createrawtransaction(inputs, outputs)
    signresult = from_node.signrawtransaction(rawtx)
    txid = from_node.sendrawtransaction(signresult["hex"], True)

    return (txid, signresult["hex"], fee)

def assert_equal(thing1, thing2):
    if thing1 != thing2:
        raise AssertionError("%s != %s"%(str(thing1),str(thing2)))
