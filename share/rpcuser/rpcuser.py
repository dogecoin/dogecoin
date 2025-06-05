#!/usr/bin/env python3
# Copyright (c) 2015-2021 The Bitcoin Core developers
# Copyright (c) 2023 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying 
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

import sys
from secrets import token_hex, token_urlsafe
import hmac

def generate_salt(size):
    """Create size byte hex salt"""
    return token_hex(size)

def generate_password():
    """Create 32 byte b64 password"""
    return token_urlsafe(32)

def password_to_hmac(salt, password):
    m = hmac.new(salt.encode('utf-8'), password.encode('utf-8'), 'SHA256')
    return m.hexdigest()

if len(sys.argv) < 2:
    sys.stderr.write('Please include username as an argument.\n')
    sys.exit(0)

username = sys.argv[1]
password = generate_password()

# Create 16 byte hex salt
salt = generate_salt(16)
password_hmac = password_to_hmac(salt, password)

print('String has been securely written to dogecoin.conf:')
with open('dogecoin.conf', 'a') as conf_file:
    conf_file.write(f'rpcauth={username}:{salt}${password_hmac}\n')
print('Your password has been securely generated. Please store it in a safe location.')
