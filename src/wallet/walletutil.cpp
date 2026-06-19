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

fs::path WalletDataFilePath(const std::string& walletName)
{
    const fs::path walletDir = GetWalletDir();
    if (IsDirectoryWalletName(walletName)) {
        return walletDir / walletName / "wallet.dat";
    }
    return walletDir / walletName;
}

bool EnsureWalletDirectoryExists(const std::string& walletName)
{
    if (!IsDirectoryWalletName(walletName)) {
        return true; // flat wallets live at wallet-dir root; nothing to mkdir
    }
    const fs::path walletPath = GetWalletDir() / walletName;
    try {
        if (!fs::exists(walletPath)) {
            fs::create_directories(walletPath);
        } else if (!fs::is_directory(walletPath)) {
            // A regular file already squats on the directory name — fail
            // hard rather than silently fall back to flat layout, which
            // would surprise the caller.
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
