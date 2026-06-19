// Copyright (c) 2017-2018 The Bitcoin Core developers
// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "wallet/walletutil.h"

#include "util.h"

fs::path GetWalletDir()
{
    // No -walletdir support yet; every wallet lives under the data
    // directory. Wallet-dir lookup is centralised here so the eventual
    // -walletdir flag (mirroring Bitcoin Core's PR #11077) is a single
    // change.
    return GetDataDir();
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
