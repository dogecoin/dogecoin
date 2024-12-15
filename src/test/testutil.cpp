// Copyright (c) 2009-2016 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "testutil.h"

#include "fs.h"

#ifdef WIN32
#include <shlobj.h>
#endif

fs::path GetTempPath() {
#if BOOST_FILESYSTEM_VERSION == 3
    return fs::temp_directory_path();
#else
    // TODO: remove when we don't support filesystem v2 anymore
    fs::path path;
#ifdef WIN32
    char pszPath[MAX_PATH] = "";

    if (GetTempPathA(MAX_PATH, pszPath))
        path = fs::path(pszPath);
#else
    path = fs::path("/tmp");
#endif
    if (path.empty() || !fs::is_directory(path)) {
        LogPrintf("GetTempPath(): failed to find temp path\n");
        return fs::path("");
    }
    return path;
#endif
}
