// Copyright (c) 2016 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_WALLET_RPCWALLET_H
#define BITCOIN_WALLET_RPCWALLET_H

#include <string>

class CRPCTable;
class CWallet;
class JSONRPCRequest;

void RegisterWalletRPCCommands(CRPCTable &t);

/** URL prefix used to route an RPC call to a specific wallet, e.g.
 *  POST /wallet/<name>/ */
extern const std::string WALLET_ENDPOINT_BASE;

/** Resolve the wallet that an RPC call should operate on. If the request
 *  URI begins with /wallet/<name>/ the named wallet is returned; otherwise
 *  the only loaded wallet is returned (or NULL when multiple are loaded
 *  and none was specified). Returns NULL when no wallets are loaded. */
CWallet *GetWalletForJSONRPCRequest(const JSONRPCRequest& request);

/** Throw JSONRPCError when the wallet is unavailable. Returns false when
 *  avoidException is true and no wallet is present (so callers may return
 *  NullUniValue from the help branch). */
bool EnsureWalletIsAvailable(CWallet * const pwallet, bool avoidException);
void EnsureWalletIsUnlocked(CWallet * const pwallet);

/** Help text suffix used by RPCs that require an unlocked wallet. */
std::string HelpRequiringPassphrase(CWallet * const pwallet);

/** Legacy single-wallet helpers, retained while call sites are migrated to
 *  the per-request wallet handle. Remove when the migration is complete. */
bool EnsureWalletIsAvailable(bool avoidException);
void EnsureWalletIsUnlocked();

#endif //BITCOIN_WALLET_RPCWALLET_H
