// Copyright (c) 2017-2018 The Bitcoin Core developers
// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "wallet/walletutil.h"

#include "tinyformat.h"
#include "util.h"

#include <mutex>

namespace {
std::mutex g_wallet_dir_mutex;
// Empty until InitWalletDir() pins it.
fs::path g_wallet_dir;

fs::path ResolveWalletDir()
{
    // -walletdir=<dir> overrides the wallet location. Otherwise wallets
    // live under <datadir>/wallets/ if that directory exists (matches
    // Bitcoin Core's default on a fresh node), else <datadir>/ itself
    // (matches legacy single-wallet nodes that have always kept
    // wallet.dat at datadir root).
    fs::path path;
    if (IsArgSet("-walletdir")) {
        path = GetArg("-walletdir", "");
        if (!fs::is_directory(path)) {
            // CWallet::Verify() refuses to start with an invalid
            // -walletdir, so this only matters for callers that run
            // before verification (unit tests, early init).
            path = GetDataDir();
        }
    } else {
        path = GetDataDir();
        if (fs::is_directory(path / "wallets")) {
            path /= "wallets";
        }
    }
    return path;
}
} // namespace

fs::path GetWalletDir()
{
    std::lock_guard<std::mutex> lock(g_wallet_dir_mutex);
    if (!g_wallet_dir.empty()) {
        return g_wallet_dir;
    }
    return ResolveWalletDir();
}

void InitWalletDir()
{
    std::lock_guard<std::mutex> lock(g_wallet_dir_mutex);
    g_wallet_dir = ResolveWalletDir();
}

void ResetWalletDirForTesting()
{
    std::lock_guard<std::mutex> lock(g_wallet_dir_mutex);
    g_wallet_dir.clear();
}

bool IsWalletDirAvailable()
{
    return fs::is_directory(GetWalletDir());
}

bool CheckWalletName(const std::string& walletName, bool for_create, std::string& error)
{
    if (walletName.empty()) {
        error = "Wallet name must not be empty";
        return false;
    }
    if (walletName.find_first_not_of('.') == std::string::npos) {
        // ".", ".." and any other all-dot name: either a directory
        // reference or (on Windows) a name the OS strips to nothing.
        error = strprintf("Wallet name \"%s\" is not a valid file name", walletName);
        return false;
    }
    for (const unsigned char c : walletName) {
        if (c < 0x20 || c == 0x7f) {
            error = strprintf("Wallet name \"%s\" contains a control character", walletName);
            return false;
        }
    }

    // Single path segment only. Directory layouts are built by us via
    // WalletDataFileName(); user input never carries separators.
    const fs::path walletPath(walletName);
    if (walletPath.has_parent_path() ||
        walletName != walletPath.stem().string() + walletPath.extension().string()) {
        error = strprintf("Wallet %s resides outside wallet directory %s", walletName, GetWalletDir().string());
        return false;
    }

    if (!for_create) {
        return true;
    }

    static const std::string non_portable = "<>:\"/\\|?*";
    const size_t bad = walletName.find_first_of(non_portable);
    if (bad != std::string::npos) {
        error = strprintf("Wallet name \"%s\" contains '%c', which is not allowed in file names on all platforms", walletName, walletName[bad]);
        return false;
    }
    const char last = walletName.back();
    if (last == '.' || last == ' ') {
        error = strprintf("Wallet name \"%s\" must not end with '%c'", walletName, last);
        return false;
    }

    // Windows reserves these device names regardless of extension:
    // "nul.dat" opens the null device, not a file.
    std::string base = walletName.substr(0, walletName.find('.'));
    for (char& c : base) {
        if (c >= 'a' && c <= 'z') c -= 'a' - 'A';
    }
    static const char* const reserved[] = {"CON", "PRN", "AUX", "NUL"};
    bool is_reserved = false;
    for (const char* r : reserved) {
        if (base == r) is_reserved = true;
    }
    if (base.size() == 4 && (base.compare(0, 3, "COM") == 0 || base.compare(0, 3, "LPT") == 0) &&
        base[3] >= '1' && base[3] <= '9') {
        is_reserved = true;
    }
    if (is_reserved) {
        error = strprintf("Wallet name \"%s\" is a reserved device name on Windows", walletName);
        return false;
    }
    return true;
}

bool IsDirectoryWalletName(const std::string& walletName)
{
    if (walletName.empty()) {
        return false;
    }
    return walletName.find('.') == std::string::npos;
}

bool CheckWalletLayout(const std::string& walletName, std::string& error)
{
    const fs::path path = GetWalletDir() / walletName;
    try {
        if (!fs::exists(path)) {
            return true;
        }
        if (IsDirectoryWalletName(walletName)) {
            if (fs::is_directory(path)) {
                return true;
            }
            if (fs::is_regular_file(path)) {
                error = strprintf("Wallet \"%s\" has no '.' in its name, so it uses the directory layout %s, "
                                  "but %s is a regular file. To keep this name, stop the node and move the file "
                                  "to %s; or rename the file to %s.dat and use that name.",
                                  walletName, (path / "wallet.dat").string(), path.string(),
                                  (path / "wallet.dat").string(), walletName);
                return false;
            }
            error = strprintf("Wallet \"%s\": %s exists but is not a directory", walletName, path.string());
            return false;
        }
        if (fs::is_directory(path)) {
            error = strprintf("Wallet \"%s\" contains a '.', so it uses the flat layout, but %s is a directory",
                              walletName, path.string());
            return false;
        }
    } catch (const fs::filesystem_error& e) {
        error = strprintf("Wallet \"%s\": cannot inspect %s: %s", walletName, path.string(), e.what());
        return false;
    }
    return true;
}

std::string WalletDataFileName(const std::string& walletName)
{
    if (!IsDirectoryWalletName(walletName)) {
        // Covers DEFAULT_WALLET_DAT ("wallet.dat"): a legacy single-wallet
        // node sees zero behaviour change.
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
            fs::create_directories(walletPath);
        }
        return fs::is_directory(walletPath);
    } catch (const fs::filesystem_error&) {
        return false;
    }
}

std::string WalletNameFromDataFilePath(const std::string& dataFilePath)
{
    // Convention from WalletDataFileName():
    //   directory wallet → "<name>/wallet.dat"
    //   flat wallet      → "<name>"
    const std::string suffix = "/wallet.dat";
    if (dataFilePath.size() > suffix.size() &&
        dataFilePath.compare(dataFilePath.size() - suffix.size(), suffix.size(), suffix) == 0) {
        return dataFilePath.substr(0, dataFilePath.size() - suffix.size());
    }
    return dataFilePath;
}
