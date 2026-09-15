#!/usr/bin/env python3
#
# linearize-hashes.py:  List blocks in a linear, no-fork version of the chain.
#
# Copyright (c) 2013-2016 The Bitcoin Core developers
# Copyright (c) 2019-2022 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
#

from __future__ import print_function
try: # Python 3
    import http.client as httplib
except ImportError: # Python 2
    import httplib
import json
import os
import re
import base64
import sys

settings = {}

##### Switch endian-ness #####
def hex_switchEndian(s):
	""" Switches the endianness of a hex string (in pairs of hex chars) """
	pairList = [s[i:i+2].encode() for i in range(0, len(s), 2)]
	return b''.join(pairList[::-1]).decode()

class BitcoinRPC:
	def __init__(self, host, port, authpair):
		self.authhdr = b"Basic " + base64.b64encode(authpair.encode('utf-8'))
		self.conn = httplib.HTTPConnection(host, port=port, timeout=30)

	def execute(self, obj):
		try:
			self.conn.request('POST', '/', json.dumps(obj),
				{ 'Authorization' : self.authhdr,
				  'Content-type' : 'application/json' })
		except ConnectionRefusedError:
			print('RPC connection refused. Check RPC settings and the server status.',
			      file=sys.stderr)
			return None

		resp = self.conn.getresponse()
		if resp is None:
			print("JSON-RPC: no response", file=sys.stderr)
			return None

		body = resp.read().decode('utf-8')
		resp_obj = json.loads(body)
		return resp_obj

	@staticmethod
	def build_request(idx, method, params):
		obj = { 'version' : '1.1',
			'method' : method,
			'id' : idx }
		if params is None:
			obj['params'] = []
		else:
			obj['params'] = params
		return obj

	@staticmethod
	def response_is_error(resp_obj):
		return 'error' in resp_obj and resp_obj['error'] is not None

def default_datadir():
	"""The directory dogecoind uses when -datadir is not given."""
	if sys.platform == 'darwin':
		return os.path.expanduser('~/Library/Application Support/Dogecoin')
	if sys.platform.startswith('win'):
		return os.path.join(os.environ.get('APPDATA', ''), 'Dogecoin')
	return os.path.expanduser('~/.dogecoin')

def read_cookie(path):
	"""Return the 'user:password' pair dogecoind wrote to its cookie file, or None."""
	try:
		with open(path, 'r') as f:
			authpair = f.read().strip()
	except (IOError, OSError):
		return None
	# The file holds exactly "__cookie__:<password>"; anything else is not a cookie.
	if ':' not in authpair:
		return None
	return authpair

def resolve_authpair(settings):
	"""Work out how to authenticate, preferring an explicit user and password.

	dogecoind writes a .cookie file into its data directory whenever rpcuser is not configured, which is how a
	locally-run node is meant to be reached now that rpcuser/rpcpassword are on their way out. See
	https://github.com/dogecoin/dogecoin/issues/1683.
	"""
	if 'rpcuser' in settings and 'rpcpassword' in settings:
		return "%s:%s" % (settings['rpcuser'], settings['rpcpassword'])

	if 'cookiefile' in settings:
		cookiepath = settings['cookiefile']
	else:
		cookiepath = os.path.join(settings.get('datadir', default_datadir()), '.cookie')

	authpair = read_cookie(cookiepath)
	if authpair is not None:
		return authpair

	print("No RPC credentials found.", file=sys.stderr)
	print("Set rpcuser and rpcpassword in the config file, or let the tool read the cookie file that", file=sys.stderr)
	print("dogecoind writes when rpcuser is not configured. Looked for the cookie at:", file=sys.stderr)
	print("  %s" % cookiepath, file=sys.stderr)
	print("Set 'datadir' to dogecoind's data directory, or 'cookiefile' to the cookie itself, if it lives", file=sys.stderr)
	print("somewhere else. For testnet the data directory is the testnet3 subdirectory.", file=sys.stderr)
	sys.exit(1)

def get_block_hashes(settings, max_blocks_per_call=10000):
	rpc = BitcoinRPC(settings['host'], settings['port'], settings['authpair'])

	height = settings['min_height']
	while height < settings['max_height']+1:
		num_blocks = min(settings['max_height']+1-height, max_blocks_per_call)
		batch = []
		for x in range(num_blocks):
			batch.append(rpc.build_request(x, 'getblockhash', [height + x]))

		reply = rpc.execute(batch)
		if reply is None:
			print('Cannot continue. Program will halt.')
			return None

		for x,resp_obj in enumerate(reply):
			if rpc.response_is_error(resp_obj):
				print('JSON-RPC: error at height', height+x, ': ', resp_obj['error'], file=sys.stderr)
				exit(1)
			assert(resp_obj['id'] == x) # assume replies are in-sequence
			if settings['rev_hash_bytes'] == 'true':
				resp_obj['result'] = hex_switchEndian(resp_obj['result'])
			print(resp_obj['result'])

		height += num_blocks

if __name__ == '__main__':
	if len(sys.argv) != 2:
		print("Usage: linearize-hashes.py CONFIG-FILE")
		sys.exit(1)

	f = open(sys.argv[1])
	for line in f:
		# skip comment lines
		m = re.search('^\s*#', line)
		if m:
			continue

		# parse key=value lines
		m = re.search('^(\w+)\s*=\s*(\S.*)$', line)
		if m is None:
			continue
		settings[m.group(1)] = m.group(2)
	f.close()

	if 'host' not in settings:
		settings['host'] = '127.0.0.1'
	if 'port' not in settings:
		settings['port'] = 22555
	if 'min_height' not in settings:
		settings['min_height'] = 0
	if 'max_height' not in settings:
		settings['max_height'] = 313000
	if 'rev_hash_bytes' not in settings:
		settings['rev_hash_bytes'] = 'false'
	settings['authpair'] = resolve_authpair(settings)

	settings['port'] = int(settings['port'])
	settings['min_height'] = int(settings['min_height'])
	settings['max_height'] = int(settings['max_height'])

	# Force hash byte format setting to be lowercase to make comparisons easier.
	settings['rev_hash_bytes'] = settings['rev_hash_bytes'].lower()

	get_block_hashes(settings)
