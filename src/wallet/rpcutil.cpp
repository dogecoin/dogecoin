// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "wallet/rpcutil.h"

boost::filesystem::path GetBackupDirFromInput(std::string strUserFilename)
{
    const boost::filesystem::path backupDir = GetBackupDir();

    if (strUserFilename != "") {
        boost::filesystem::path p(strUserFilename);
        boost::filesystem::path filename = p.filename();

        if (!filename.empty())
            return backupDir / filename;
    }

    return backupDir;
}
