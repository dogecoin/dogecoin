#!/usr/bin/env python3
import requests

def check_rpc_connection():
    """Simple check for local Dogecoin RPC availability."""
    try:
        response = requests.post(
            "http://127.0.0.1:22555",
            json={"method": "getblockcount", "params": []},
            auth=("dogeuser", "dogepassword"),
            timeout=3
        )
        if response.status_code == 200:
            print("✅ RPC connection successful!")
        else:
            print(f"⚠️ Unexpected status: {response.status_code}")
    except Exception as e:
        print(f"❌ RPC connection failed: {e}")

if __name__ == "__main__":
    check_rpc_connection()
