// Copyright (c) 2017-2018 The Bitcoin Core developers
// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef DOGECOIN_WALLET_WALLETUTIL_H
#define DOGECOIN_WALLET_WALLETUTIL_H

#include "fs.h"

#include <string>

//! Get the path of the wallet directory.
//!
//! Currently this just returns GetDataDir(). When `-walletdir` config support
//! is added the lookup moves here. Centralising it now keeps every wallet
//! callsite that needs "where does <wallet_name> live" pointed at one place,
//! so the eventual `-walletdir` follow-up is a single-file change.
fs::path GetWalletDir();

//! True when the wallet name should be interpreted as a directory wallet
//! (i.e. `<walletdir>/<name>/wallet.dat`).
//!
//! Heuristic, matching Bitcoin Core post-PR #11687:
//!   - empty            → flat (legacy default-wallet behaviour)
//!   - contains a `.`   → flat (e.g. `unsandbox.dat`, `legacy.bak`)
//!   - otherwise        → directory (e.g. `unsandbox`)
//!
//! The dot-suffix detection preserves backwards compatibility: any wallet
//! file users have been carrying with an explicit extension keeps being
//! opened as a flat file in the wallet dir root.
bool IsDirectoryWalletName(const std::string& walletName);

//! Resolve a user-facing wallet name into the path BDB should open.
//!
//! For directory wallets returns `<walletdir>/<name>/wallet.dat`. For flat
//! wallets returns `<walletdir>/<name>` (where `<name>` already carries its
//! extension, e.g. `unsandbox.dat`).
//!
//! Callers must ensure the parent directory exists before opening the
//! database; see EnsureWalletDirectoryExists() below.
fs::path WalletDataFilePath(const std::string& walletName);

//! Create the directory for a directory-layout wallet, if needed.
//!
//! No-op when walletName is a flat wallet. Returns false if the directory
//! could not be created (filesystem error, permissions, etc.); the caller
//! should surface this to the user as a wallet-creation failure.
bool EnsureWalletDirectoryExists(const std::string& walletName);

//! Inverse of WalletDataFilePath() — recover the user-facing wallet name
//! from an internal BDB file path.
//!
//! For "unsandbox/wallet.dat" returns "unsandbox".
//! For "unsandbox.dat" returns "unsandbox.dat" (flat file = name).
//!
//! Used by CWallet::GetName() so listwallets and the /wallet/<name>/ URL
//! router speak the user-facing name regardless of on-disk layout.
std::string WalletNameFromDataFilePath(const std::string& dataFilePath);

#endif // DOGECOIN_WALLET_WALLETUTIL_H
