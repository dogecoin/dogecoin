// Copyright (c) 2017-2018 The Bitcoin Core developers
// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "wallet/walletutil.h"

#include "util.h"

fs::path GetWalletDir()
{
    // -walletdir=<dir> overrides the wallet location. Otherwise wallets
    // live under <datadir>/wallets/ if that directory exists (matches
    // Bitcoin Core's default on a fresh node), else <datadir>/ itself
    // (matches legacy single-wallet nodes that have always kept
    // wallet.dat at datadir root).
    //
    // The three-way resolution lets users migrate by creating
    // <datadir>/wallets/ and moving their wallets in without changing
    // any flags; legacy nodes that never created the directory keep
    // operating exactly as before.
    fs::path path;
    if (IsArgSet("-walletdir")) {
        path = GetArg("-walletdir", "");
        if (!fs::is_directory(path)) {
            // Fall back to default; the daemon's own validation in init.cpp
            // logs a clearer error before we get here in practice, but in
            // case this is called without that gate we still want a sane
            // default rather than a non-existent path.
            path = GetDataDir();
        }
    } else {
        path = GetDataDir();
        // Adopt the Bitcoin Core post-0.18 convention: if a "wallets"
        // subdirectory exists, prefer it. This lets a user split chain
        // data from wallet data by mkdir-ing wallets/ themselves.
        if (fs::is_directory(path / "wallets")) {
            path /= "wallets";
        }
    }
    return path;
}

bool IsDirectoryWalletName(const std::string& walletName)
{
    if (walletName.empty()) {
        return false;
    }
    // A `.` anywhere in the name keeps the historical flat-file
    // semantics (`unsandbox.dat`, `archive.bak`, etc.). Names without
    // any dot are treated as directory wallets, matching how Bitcoin
    // Core decides createwallet's on-disk layout post-PR #11687.
    return walletName.find('.') == std::string::npos;
}

std::string WalletDataFileName(const std::string& walletName)
{
    if (!IsDirectoryWalletName(walletName)) {
        // Dot-suffixed name: always flat. Covers DEFAULT_WALLET_DAT
        // ("wallet.dat") so a legacy single-wallet node sees zero
        // behaviour change after upgrading to a build carrying this
        // backport.
        return walletName;
    }
    // Migration-friendly resolution for no-extension names:
    //   1. If a regular file already sits at <walletdir>/<name>, treat
    //      it as a flat wallet. Lets users adopt the new naming
    //      convention (drop the .dat) WITHOUT a mkdir+mv beforehand:
    //      mv unsandbox.dat unsandbox is enough.
    //   2. Otherwise resolve to <name>/wallet.dat — the directory
    //      layout new wallets default to.
    const fs::path candidateFlat = GetWalletDir() / walletName;
    if (fs::exists(candidateFlat) && fs::is_regular_file(candidateFlat)) {
        return walletName;
    }
    return walletName + "/wallet.dat";
}

fs::path WalletDataFilePath(const std::string& walletName)
{
    return GetWalletDir() / WalletDataFileName(walletName);
}

bool EnsureWalletDirectoryExists(const std::string& walletName)
{
    if (!IsDirectoryWalletName(walletName)) {
        return true; // flat wallets live at wallet-dir root; nothing to mkdir
    }
    const fs::path walletPath = GetWalletDir() / walletName;
    try {
        if (!fs::exists(walletPath)) {
            // No file or directory at this name yet — create the wallet
            // directory. CreateWalletFromFile() will populate it.
            fs::create_directories(walletPath);
        } else if (fs::is_regular_file(walletPath)) {
            // A flat wallet file already squats on the name. Do NOT
            // mkdir — the migration-friendly path in WalletDataFilePath
            // will resolve to the flat file instead. Returning success
            // keeps the open flow going with flat semantics.
            return true;
        } else if (!fs::is_directory(walletPath)) {
            // Something exotic (symlink to nothing, socket, etc.) —
            // refuse rather than risk silent data loss.
            return false;
        }
    } catch (const fs::filesystem_error&) {
        return false;
    }
    return true;
}

std::string WalletNameFromDataFilePath(const std::string& dataFilePath)
{
    // Convention from WalletDataFilePath():
    //   directory wallet → "<name>/wallet.dat"
    //   flat wallet      → "<name>"
    const std::string suffix = "/wallet.dat";
    if (dataFilePath.size() > suffix.size() &&
        dataFilePath.compare(dataFilePath.size() - suffix.size(), suffix.size(), suffix) == 0) {
        return dataFilePath.substr(0, dataFilePath.size() - suffix.size());
    }
    return dataFilePath;
}
