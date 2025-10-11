#!/usr/bin/env python3
"""
Simple RPC connection health check for Dogecoin Core nodes.
Educational example for GitHub contribution training.
"""

import sys
import json
import urllib.request

RPC_URL = "http://127.0.0.1:22555"  # default Dogecoin mainnet RPC
RPC_USER = "dogeuser"
RPC_PASS = "dogepassword"

def call_rpc(method):
    data = json.dumps({"method": method, "params": [], "id": 1}).encode()
    req = urllib.request.Request(RPC_URL, data)
    req.add_header("Content-Type", "application/json")
    auth = f"{RPC_USER}:{RPC_PASS}".encode("utf-8")
    base64_auth = urllib.request.base64.b64encode(auth).decode("utf-8")
    req.add_header("Authorization", f"Basic {base64_auth}")

    try:
        with urllib.request.urlopen(req, timeout=3) as response:
            result = json.loads(response.read().decode())
            return result
    except Exception as e:
        return {"error": str(e)}

if __name__ == "__main__":
    res = call_rpc("getblockcount")
    if "error" in res:
        print("❌ RPC connection failed:", res["error"])
        sys.exit(1)
    print("✅ RPC OK. Current block height:", res["result"])
