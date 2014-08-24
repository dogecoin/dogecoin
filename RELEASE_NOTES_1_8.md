# Dogecoin Core 1.8
===================

Dogecoin Core 1.8 introduces AuxPoW from block 371,337. AuxPoW is a technology
which enables miners to submit work done while mining other coins, as work
on the Dogecoin block chain. Dogecoin Core 1.8 also enables payment protocol
support for Dogecoin (note that Dogecoin's implementation of payment protocol is
not compatible with Bitcoin's protocol). Lastly, 1.8  also includes all fixes
from Bitcoin Core 0.9.2, whose release notes you can read at
https://bitcoin.org/en/release/v0.9.2 (this is a summary, and the release includes
around 150 bugfixes from Bitcoin Core).

Note that all users MUST upgrade to 1.8. If you're unable to update before the
switchover block (371,337, expected around 12 September 2014), you MUST update
as soon as possible afterwards in order to be able to continue to use Dogecoin.
Users remaining on the previous client **will not** be able to receive or spend
Dogecoin (with the exception of with other nodes who have not upgraded).

## Upgrading

Before upgrading, back up your wallet.dat file just in case of any problems; you
can back up the wallet.dat file by using the "Backup Wallet" option from the "File"
menu. Ensure your existing wallet software is closed before upgrading.

Note that a full reindex is required as part of the upgrade to 1.8, and this
will typically take around an hour to complete. If you are running the Qt client
you will be prompted to reindex when first running the software, while if you
use "dogecoind" you must run it the first time with the "-reindex" command line
option.

To ensure a smooth switchover to AuxPoW, 1.8 has safe mode disabled 1 hour before
until 24 hours after the switch block. This ensures the actually legit fork will not
trigger safe mode and halt mining. If you don't want to have this behavior, then
refer to the 1.8-safemode branch. This one doesn't include the patch to disable safe mode.

## Downgrading

As 1.8 includes a substantial change to the mining protocol, downgrading to previous
clients is not possible.

## AuxPoW

AuxPoW is enabled from block 371,337. Until that block the RPC commands for using AuxPoW
will not function, and AuxPoW blocks will be rejected.

AuxPoW benefits Dogecoin in two ways; firstly, the effective hashrate of the coin is
increased by these additional miners, making it harder to perform a 51% attack against
the coin, and secondly it reduces conflict for resources (Scrypt miners), demotivating
multipools from switching between Dogecoin and other Scrypt coins.

AuxPoW blocks are mined using the "getauxblock" and "getworkaux" RPC commands. Support
for AuxPoW mining is included in Powerpool ( https://github.com/simplecrypto/powerpool/ )
and p2pool ( http://p2pool.in/ ).

In comparison to the AuxPoW implementation in United Scrypt Coin, Dogecoin uses a
different coinbase transaction input script format which includes block height in
compliance with BIP0034.

## Payment Protocol

Dogecoin Core 1.8 adds payment protocol support, which is used to streamline payments
being made to merchants.

This protocol is defined in DIP0070-DIP0072, based on the BIP standards with the same
assigned numbers. DIP standards can be found at https://github.com/dogecoin/dips/ .
The key differencs between the Bitcoin and Dogecoin payment protocols are that
the Dogecoin payment request uses a "genesis" field containing the hash of the network's
genesis block to identify networks instead of the "network" field. Further, MIME types
for the request, payment and response are modified to identify the files as distinct
types from the Bitcoin files.

## Transaction Fees

As of 1.8 all transactions have fees applied by defaults, with no exemptions made for
"old" coins. This ensures fees are consistent, while remaining extremely competitive
(typically 1 Doge, or around $0.00012 per transaction).

## RPC Allow IP

The format of IP masks supplied to the "rpcallowip" command line option has changed.
It no longer accepts subnets like '192.168.*.*', and the format '192.168/16" should
be used instead.

## Other Changes

The Chinese and Korean translations have been updated.

Protocol version has been updated to 70003, meaning that a later release of the client
can block access to all old clients (70002 and below).

In case of an error reading the block database from disk, the database cursor could be
left open, which caused an assertion error later. The database cursor is now correctly
disposed of in all cases.

Tweaked trigger conditions for safe mode to take into account the much faster block time
of Dogecoin compared to Bitcoin. This should lead to less false positives.

A problem in parsing mangled dogecoin: URIs under Windows has been resolved, and as
part of this work network detection for dogecoin: URIs is now more robust.
