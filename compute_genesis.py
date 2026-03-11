#!/usr/bin/env python3
import hashlib
import struct
import time

# ScrapCoin genesis parameters
TIMESTAMP = "ScrapCoin Genesis Block - Decentralized Recycling Economy"
GENESIS_REWARD = 50 * 100000000  # 50 SCRAP in scralets
BITS = 0x1e0ffff0
VERSION = 1

# The pubkey from Dogecoin genesis (keeping same for compatibility)
PUBKEY_HEX = "040184710fa689ad5023690c80f3a49c8f13f8d45b8c857fbcbc8bc4a8e4d3eb4b10f4d4604fa08dce601aaf0f470216fe1b51850b4acf21b179c45070ac7b03a9"

def double_sha256(data):
    return hashlib.sha256(hashlib.sha256(data).digest()).digest()

def create_coinbase_tx(nonce, timestamp_bytes, nTime):
    # Build scriptSig: <486604799> <4> <timestamp>
    script_sig = b''
    script_sig += struct.pack("<I", 486604799)  # First push: 486604799
    script_sig += struct.pack("<I", 4)  # Second push: 4 (length indicator)
    script_sig += timestamp_bytes  # Third push: timestamp bytes
    
    # Build scriptPubKey: <pubkey> OP_CHECKSIG
    script_pubkey = bytes.fromhex(PUBKEY_HEX) + b'\xac'  # 0xac = OP_CHECKSIG
    
    # Serialize transaction
    tx = b''
    tx += struct.pack("<I", VERSION)  # nVersion
    
    # Input count
    tx += struct.pack("<B", 1)
    
    # Input: COutPoint
    tx += b'\x00' * 32  # prev tx hash (null)
    tx += struct.pack("<I", 0xffffffff)  # prev index (-1)
    
    # Input: scriptSig
    script_sig_len = len(script_sig)
    tx += struct.pack("<B", script_sig_len)
    tx += script_sig
    
    # Input: sequence
    tx += struct.pack("<I", 0xffffffff)
    
    # Output count
    tx += struct.pack("<B", 1)
    
    # Output: CTransactionOutput
    tx += struct.pack("<Q", GENESIS_REWARD)  # nValue
    # Output: scriptPubKey
    script_pubkey_len = len(script_pubkey)
    tx += struct.pack("<B", script_pubkey_len)
    tx += script_pubkey
    
    # Lock time
    tx += struct.pack("<I", nTime)
    
    return tx

def compute_merkle_root(tx):
    tx_hash = double_sha256(tx)
    return tx_hash

def mine_genesis_block(nonce_start=0, max_nonce=10000000):
    timestamp_bytes = TIMESTAMP.encode('utf-8')
    nTime = int(time.time())  # Use current time
    
    for nonce in range(nonce_start, max_nonce):
        # Create coinbase transaction
        coinbase_tx = create_coinbase_tx(nonce, timestamp_bytes, nTime)
        merkle_root = compute_merkle_root(coinbase_tx)
        
        # Build block header
        block_header = b''
        block_header += struct.pack("<I", VERSION)
        block_header += b'\x00' * 32  # prev block hash (null)
        block_header += merkle_root
        block_header += struct.pack("<I", nTime)
        block_header += struct.pack("<I", BITS)
        block_header += struct.pack("<I", nonce)
        
        block_hash = double_sha256(block_header)
        
        # Check if hash meets target
        # Target is compact bits format - need to expand
        # For 0x1e0ffff0, target is ~ (2^208)
        # We can check by comparing leading zeros
        hash_int = int.from_bytes(block_hash[::-1], 'big')
        
        # Extract target from bits
        size = (BITS >> 24) & 0xff
        if size == 1:
            target = BITS & 0x00ffffff
        else:
            target = (BITS & 0x00ffffff) * (1 << (8 * (size - 3)))
        
        if hash_int <= target:
            print(f"Genesis block found!")
            print(f"Nonce: {nonce}")
            print(f"nTime: {nTime}")
            print(f"Block Hash (hex): {block_hash[::-1].hex()}")
            print(f"Merkle Root (hex): {merkle_root[::-1].hex()}")
            return nonce, block_hash[::-1].hex(), merkle_root[::-1].hex(), nTime
        
        if nonce % 10000 == 0:
            print(f"Tried {nonce} nonces...")
    
    print(f"Failed to find genesis block after {max_nonce} attempts")
    return None, None, None, None

if __name__ == "__main__":
    nonce, block_hash, merkle_root, nTime = mine_genesis_block()
    if nonce:
        print("\n=== Add to chainparams.cpp ===")
        print(f'genesis = CreateGenesisBlock({nTime}, {nonce}, 0x{BITS:08x}, 1, 50 * COIN);')
        print(f'assert(consensus.hashGenesisBlock == uint256S("0x{block_hash}"));')
        print(f'assert(genesis.hashMerkleRoot == uint256S("0x{merkle_root}"));')