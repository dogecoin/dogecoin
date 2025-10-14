#!/usr/bin/env python3
"""
Dogecoin Core QA Utility
--------------------------------
Educational example by Marta.
Checks whether the Dogecoin node has active peer connections.
"""

import json
import urllib.request

RPC_URL = "http://127.0.0.1:22555"
AUTH_HEADER = "Basic ZG9nZXVzZXI6ZG9nZXBhc3N3b3Jk"  # dogeuser:dogepassword (example only)

def rpc_call(method):
    data = json.dumps({"method": method, "params": [], "id": 1}).encode()
    req = urllib.request.Request(
        RPC_URL, data,
        headers={"Content-Type": "application/json", "Authorization": AUTH_HEADER}
    )
    try:
        with urllib.request.urlopen(req, timeout=3) as res:
            return json.loads(res.read().decode())
    except Exception as e:
        return {"error": str(e)}

if __name__ == "__main__":
    response = rpc_call("getconnectioncount")
    if "error" in response:
        print("❌ Connection error:", response["error"])
    else:
        peers = response["result"]
        print(f"✅ Node connected to {peers} peer(s).")
