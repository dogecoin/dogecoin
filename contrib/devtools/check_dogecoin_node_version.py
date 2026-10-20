#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# check_dogecoin_node_version.py
# Simple developer utility for verifying Dogecoin Core node version via RPC
#
# Author: Contributor <contributor@example.com>
# License: MIT
#
# Usage:
#   python3 check_dogecoin_node_version.py --rpcuser=<user> --rpcpassword=<pass> --rpcport=22555
#

import argparse
import json
import requests
from requests.auth import HTTPBasicAuth

def get_node_version(rpc_user, rpc_password, rpc_port):
    url = f"http://127.0.0.1:{rpc_port}/"
    payload = json.dumps({"jsonrpc": "1.0", "id": "check", "method": "getnetworkinfo", "params": []})
    headers = {"content-type": "text/plain"}

    try:
        response = requests.post(url, headers=headers, data=payload, auth=HTTPBasicAuth(rpc_user, rpc_password))
        response.raise_for_status()
        result = response.json()["result"]
        version = result.get("version", "unknown")
        subversion = result.get("subversion", "unknown")
        protocol = result.get("protocolversion", "unknown")

        print(f"✅ Node version: {version} | {subversion}")
        print(f"📡 Protocol version: {protocol}")

    except Exception as e:
        print(f"❌ Error connecting to Dogecoin node: {e}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Check Dogecoin node version via RPC")
    parser.add_argument("--rpcuser", required=True, help="RPC username")
    parser.add_argument("--rpcpassword", required=True, help="RPC password")
    parser.add_argument("--rpcport", type=int, default=22555, help="RPC port (default: 22555)")
    args = parser.parse_args()

    get_node_version(args.rpcuser, args.rpcpassword, args.rpcport)
