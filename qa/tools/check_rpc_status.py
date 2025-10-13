#!/usr/bin/env python3
"""
Dogecoin Core - RPC Connection Status Tool
Educational example by Marta.
"""

import sys
import json
import urllib.request

RPC_URL = "http://127.0.0.1:22555"  # Default Dogecoin mainnet RPC
RPC_USER = "dogeuser"
RPC_PASS = "dogepassword"

def rpc_call(method):
    payload = json.dumps({"method": method, "params": [], "id": 1}).encode()
    req = urllib.request.Request(RPC_URL, payload, headers={
        "Content-Type": "application/json",
        "Authorization": "Basic ZG9nZXVzZXI6ZG9nZXBhc3N3b3Jk"
    })

    try:
        with urllib.request.urlopen(req, timeout=3) as resp:
            data = json.loads(resp.read().decode())
            return data
    except Exception as e:
        return {"error": str(e)}

if __name__ == "__main__":
    result = rpc_call("getblockcount")
    if "error" in result:
        print("❌ RPC connection failed:", result["error"])
        sys.exit(1)
    print("✅ RPC OK — current block height:", result["result"])
