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
//! Resolution order: `-walletdir=<dir>` if set, else `<datadir>/wallets`
//! if that directory exists, else `<datadir>`.
//!
//! Once InitWalletDir() has run (during wallet verification at startup)
//! the result is fixed for the life of the process. Creating or removing
//! `<datadir>/wallets` (or the -walletdir target) at runtime therefore
//! never silently redirects wallet I/O to a different directory; callers
//! that are about to touch disk after startup use IsWalletDirAvailable()
//! to report a vanished directory instead.
fs::path GetWalletDir();

//! Resolve the wallet directory from the current arguments and pin it.
//! Called once from CWallet::Verify() after -walletdir has been validated.
void InitWalletDir();

//! Drop a pinned wallet directory so GetWalletDir() resolves live again.
//! Test-only: unit tests change -walletdir / datadir between cases.
void ResetWalletDirForTesting();

//! True when GetWalletDir() still refers to an existing directory.
bool IsWalletDirAvailable();

//! Validate a user-supplied wallet name.
//!
//! Always rejected (load and create):
//!   - the empty name
//!   - names made only of dots (".", "..", "....")
//!   - names with control characters
//!   - names that are not a single path segment ("../x", "/etc/passwd",
//!     "sub/dir/w"); error text says the wallet "resides outside" the
//!     wallet directory
//!
//! Additionally rejected when `for_create` is true, so a wallet created
//! on one platform can be opened on every platform we ship:
//!   - characters Windows refuses in file names:  < > : " / \ | ? *
//!   - a trailing '.' or ' ' (Windows silently strips them)
//!   - Windows reserved device names (CON, PRN, AUX, NUL, COM1-9,
//!     LPT1-9), with or without an extension
//!
//! Loading keeps the looser rule so an existing wallet whose name is legal
//! on its host filesystem keeps opening after an upgrade.
bool CheckWalletName(const std::string& walletName, bool for_create, std::string& error);

//! True when the wallet name uses the directory layout
//! (`<walletdir>/<name>/wallet.dat`).
//!
//! The name alone decides, never the state of the disk:
//!   - contains a `.`   → flat file at `<walletdir>/<name>`
//!                        (e.g. `wallet.dat`, `unsandbox.dat`)
//!   - otherwise        → directory at `<walletdir>/<name>/`
//!                        (e.g. `unsandbox`)
//!
//! This keeps every legacy `*.dat` wallet (including DEFAULT_WALLET_DAT)
//! opening exactly as before, and means the same configuration always
//! resolves to the same file between runs. Note this deliberately differs
//! from Bitcoin Core, which picks the layout from whether a regular file
//! already exists at the path.
bool IsDirectoryWalletName(const std::string& walletName);

//! Check that what is on disk at the wallet's path matches the layout its
//! name implies. Fails (with an actionable message) when a directory-layout
//! name is squatted by a regular file, or a flat-layout name is a
//! directory. A path that does not exist yet passes: the wallet is new.
bool CheckWalletLayout(const std::string& walletName, std::string& error);

//! Resolve a user-facing wallet name into the BDB file string relative to
//! GetWalletDir(): `<name>/wallet.dat` for directory wallets, `<name>` for
//! flat wallets. Pure function of the name.
std::string WalletDataFileName(const std::string& walletName);

//! Absolute path version of WalletDataFileName() — prepends GetWalletDir().
//! Useful for fs::exists / log lines / error messages.
fs::path WalletDataFilePath(const std::string& walletName);

//! Create the directory for a directory-layout wallet, if needed.
//!
//! No-op when walletName is a flat wallet. Returns false if the directory
//! could not be created, or if something other than a directory already
//! occupies the path; callers run CheckWalletLayout() first to give the
//! user a precise error for the latter.
bool EnsureWalletDirectoryExists(const std::string& walletName);

//! Inverse of WalletDataFileName() — recover the user-facing wallet name
//! from an internal BDB file path.
//!
//! For "unsandbox/wallet.dat" returns "unsandbox".
//! For "unsandbox.dat" returns "unsandbox.dat" (flat file = name).
//!
//! Used by CWallet::GetName() so listwallets and the /wallet/<name>/ URL
//! router speak the user-facing name regardless of on-disk layout.
std::string WalletNameFromDataFilePath(const std::string& dataFilePath);

#endif // DOGECOIN_WALLET_WALLETUTIL_H
