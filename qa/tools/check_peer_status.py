#!/usr/bin/env python3
# Copyright (c) 2025 Marta
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""
Dogecoin Core QA Utility
--------------------------------
Educational example for school project.
This tool checks whether a Dogecoin Core node has any active peer connections
using JSON-RPC interface.
"""

import json
import urllib.request

RPC_URL = "http://127.0.0.1:22555"
AUTH = "dogeuser:dogepassword"  # Example credentials (do not use in production)

def rpc_call(method):
    payload = json.dumps({"method": method, "params": [], "id": 1}).encode()
    headers = {
        "Content-Type": "application/json",
        "Authorization": "Basic ZG9nZXVzZXI6ZG9nZXBhc3N3b3Jk"
    }
    req = urllib.request.Request(RPC_URL, data=payload, headers=headers)
    try:
        with urllib.request.urlopen(req, timeout=3) as res:
            return json.loads(res.read().decode())
    except Exception as e:
        return {"error": str(e)}

if __name__ == "__main__":
    result = rpc_call("getconnectioncount")
    if "error" in result:
        print("❌ Error checking peer connections:", result["error"])
    else:
        print(f"✅ Node connected to {result['result']} peer(s).")
