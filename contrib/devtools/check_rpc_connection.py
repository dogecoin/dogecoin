#!/usr/bin/env python3
import json, sys
import urllib.request

def check_rpc_connection(host="127.0.0.1", port=22555):
    try:
        req = urllib.request.Request(
            f"http://{host}:{port}",
            data=json.dumps({"method": "getblockcount"}).encode(),
            headers={"Content-Type": "application/json"}
        )
        urllib.request.urlopen(req, timeout=2)
        print("✅ RPC connection successful.")
    except Exception as e:
        print(f"❌ RPC connection failed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    check_rpc_connection()
