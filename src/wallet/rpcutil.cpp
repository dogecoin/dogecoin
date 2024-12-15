// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "wallet/rpcutil.h"

#include "fs.h"

fs::path GetBackupDirFromInput(std::string strUserFilename)
{
    const fs::path backupDir = GetBackupDir();

    if (strUserFilename != "") {
        fs::path p(strUserFilename);
        fs::path filename = p.filename();

        if (!filename.empty())
            return backupDir / filename;
    }

    return backupDir;
}
