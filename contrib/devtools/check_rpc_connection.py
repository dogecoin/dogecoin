#!/usr/bin/env python3
"""
check_rpc_connection.py — simple RPC connectivity tester for Dogecoin Core
"""

import json
import requests
from requests.auth import HTTPBasicAuth

RPC_USER = "dogeuser"
RPC_PASSWORD = "dogepass"
RPC_PORT = 22555
RPC_HOST = "127.0.0.1"

def test_rpc_connection():
    url = f"http://{RPC_HOST}:{RPC_PORT}"
    headers = {"content-type": "application/json"}
    payload = json.dumps({"method": "getblockcount", "params": [], "id": 1})

    try:
        response = requests.post(url, headers=headers, data=payload, auth=HTTPBasicAuth(RPC_USER, RPC_PASSWORD))
        if response.status_code == 200:
            result = response.json()
            print(f"✅ RPC connection successful. Current block height: {result.get('result')}")
        else:
            print(f"❌ RPC error: HTTP {response.status_code}")
    except Exception as e:
        print(f"⚠️  Connection failed: {e}")

if __name__ == "__main__":
    test_rpc_connection()
