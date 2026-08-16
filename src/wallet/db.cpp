// Copyright (c) 2009-2010 Satoshi Nakamoto
// Copyright (c) 2009-2016 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "db.h"

#include "addrman.h"
#include "fs.h"
#include "hash.h"
#include "protocol.h"
#include "util.h"
#include "utilstrencodings.h"
#include "wallet/walletutil.h"

#include <stdint.h>

#ifndef WIN32
#include <sys/stat.h>
#endif

#include <boost/thread.hpp>
#include <boost/version.hpp>

using namespace std;

//
// Multi-env machinery
//
// Process-wide lock guarding g_dbenvs (the env registry) and every
// touch of per-env mutable state (mapDb, mapFileUseCount, the BDB
// open/close lifecycle). Lives at namespace level — replaces the
// per-CDBEnv `cs_db` member from the singleton era.
CCriticalSection cs_db;

namespace {
// Map from env-directory string → open environment for that directory.
// One env per unique env_directory.string(); directory-layout wallets
// (<walletdir>/<name>/wallet.dat) each get their own env at
// <walletdir>/<name>/. Flat wallets (<walletdir>/<name>.dat) share
// one env at <walletdir>/.
std::map<std::string, CDBEnv> g_dbenvs;
} // namespace

CDBEnv* GetWalletEnv(const fs::path& wallet_path, std::string& database_filename)
{
    fs::path env_directory = wallet_path.parent_path();
    if (env_directory.empty()) {
        // Bare filename — assume legacy "single env at walletdir" semantics.
        env_directory = GetWalletDir();
    }
    database_filename = wallet_path.filename().string();
    LOCK(cs_db);
    return &g_dbenvs.emplace(std::piecewise_construct,
                              std::forward_as_tuple(env_directory.string()),
                              std::forward_as_tuple(env_directory)).first->second;
}

void FlushAllDbEnvs(bool fShutdown)
{
    LOCK(cs_db);
    for (auto& kv : g_dbenvs) {
        kv.second.Flush(fShutdown);
    }
}

//
// CDB
//

void CDBEnv::Close()
{
    if (!fDbEnvInit)
        return;

    fDbEnvInit = false;

    // Close any Db handles still alive in this env. The assert catches
    // refcount bugs in unloadwallet / shutdown — leaving an open handle
    // when we tear down the env risks on-disk corruption.
    for (auto& db : mapDb) {
        auto count = mapFileUseCount.find(db.first);
        assert(count == mapFileUseCount.end() || count->second == 0);
        if (db.second) {
            db.second->close(0);
            delete db.second;
            db.second = nullptr;
        }
    }

    int ret = dbenv->close(0);
    if (ret != 0)
        LogPrintf("CDBEnv::Close: Error %d shutting down env at %s: %s\n", ret, strPath, DbEnv::strerror(ret));
    if (!fMockDb)
        DbEnv((u_int32_t)0).remove(strPath.c_str(), 0);
}

void CDBEnv::Reset()
{
    delete dbenv;
    dbenv = new DbEnv(DB_CXX_NO_EXCEPTIONS);
    fDbEnvInit = false;
    fMockDb = false;
}

CDBEnv::CDBEnv(const fs::path& dir_path) : strPath(dir_path.string()), dbenv(NULL)
{
    Reset();
}

CDBEnv::~CDBEnv()
{
    Close();
    delete dbenv;
    dbenv = NULL;
}

bool CDBEnv::Open(bool retry)
{
    if (fDbEnvInit)
        return true;

    boost::this_thread::interruption_point();

    fs::path pathIn = strPath;
    fs::path pathLogDir = pathIn / "database";
    TryCreateDirectory(pathLogDir);
    fs::path pathErrorFile = pathIn / "db.log";
    LogPrintf("CDBEnv::Open: LogDir=%s ErrorFile=%s\n", pathLogDir.string(), pathErrorFile.string());

    unsigned int nEnvFlags = 0;
    if (GetBoolArg("-privdb", DEFAULT_WALLET_PRIVDB))
        nEnvFlags |= DB_PRIVATE;

    dbenv->set_lg_dir(pathLogDir.string().c_str());
    dbenv->set_cachesize(0, 0x100000, 1); // 1 MiB should be enough for just the wallet
    dbenv->set_lg_bsize(0x10000);
    dbenv->set_lg_max(1048576);
    dbenv->set_lk_max_locks(40000);
    dbenv->set_lk_max_objects(40000);
    dbenv->set_errfile(fsbridge::fopen(pathErrorFile, "a")); /// debug
    dbenv->set_flags(DB_AUTO_COMMIT, 1);
    dbenv->set_flags(DB_TXN_WRITE_NOSYNC, 1);
    dbenv->log_set_config(DB_LOG_AUTO_REMOVE, 1);
    int ret = dbenv->open(strPath.c_str(),
                         DB_CREATE |
                             DB_INIT_LOCK |
                             DB_INIT_LOG |
                             DB_INIT_MPOOL |
                             DB_INIT_TXN |
                             DB_THREAD |
                             DB_RECOVER |
                             nEnvFlags,
                         S_IRUSR | S_IWUSR);
    if (ret != 0)
        return error("CDBEnv::Open: Error %d opening database environment: %s\n", ret, DbEnv::strerror(ret));

    fDbEnvInit = true;
    fMockDb = false;
    return true;
}

void CDBEnv::MakeMock()
{
    if (fDbEnvInit)
        throw runtime_error("CDBEnv::MakeMock: Already initialized");

    boost::this_thread::interruption_point();

    LogPrint("db", "CDBEnv::MakeMock\n");

    dbenv->set_cachesize(1, 0, 1);
    dbenv->set_lg_bsize(10485760 * 4);
    dbenv->set_lg_max(10485760);
    dbenv->set_lk_max_locks(10000);
    dbenv->set_lk_max_objects(10000);
    dbenv->set_flags(DB_AUTO_COMMIT, 1);
    dbenv->log_set_config(DB_LOG_IN_MEMORY, 1);
    int ret = dbenv->open(NULL,
                         DB_CREATE |
                             DB_INIT_LOCK |
                             DB_INIT_LOG |
                             DB_INIT_MPOOL |
                             DB_INIT_TXN |
                             DB_THREAD |
                             DB_PRIVATE,
                         S_IRUSR | S_IWUSR);
    if (ret > 0)
        throw runtime_error(strprintf("CDBEnv::MakeMock: Error %d opening database environment.", ret));

    fDbEnvInit = true;
    fMockDb = true;
}

CDBEnv::VerifyResult CDBEnv::Verify(const std::string& strFile, bool (*recoverFunc)(CDBEnv& dbenv, const std::string& strFile))
{
    LOCK(cs_db);
    assert(mapFileUseCount.count(strFile) == 0);

    Db db(dbenv, 0);
    int result = db.verify(strFile.c_str(), NULL, NULL, 0);
    if (result == 0)
        return VERIFY_OK;
    else if (recoverFunc == NULL)
        return RECOVER_FAIL;

    // Try to recover:
    bool fRecovered = (*recoverFunc)(*this, strFile);
    return (fRecovered ? RECOVER_OK : RECOVER_FAIL);
}

/* End of headers, beginning of key/value data */
static const char *HEADER_END = "HEADER=END";
/* End of key/value data */
static const char *DATA_END = "DATA=END";

bool CDBEnv::Salvage(const std::string& strFile, bool fAggressive, std::vector<CDBEnv::KeyValPair>& vResult)
{
    LOCK(cs_db);
    assert(mapFileUseCount.count(strFile) == 0);

    u_int32_t flags = DB_SALVAGE;
    if (fAggressive)
        flags |= DB_AGGRESSIVE;

    stringstream strDump;

    Db db(dbenv, 0);
    int result = db.verify(strFile.c_str(), NULL, &strDump, flags);
    if (result == DB_VERIFY_BAD) {
        LogPrintf("CDBEnv::Salvage: Database salvage found errors, all data may not be recoverable.\n");
        if (!fAggressive) {
            LogPrintf("CDBEnv::Salvage: Rerun with aggressive mode to ignore errors and continue.\n");
            return false;
        }
    }
    if (result != 0 && result != DB_VERIFY_BAD) {
        LogPrintf("CDBEnv::Salvage: Database salvage failed with result %d.\n", result);
        return false;
    }

    // Format of bdb dump is ascii lines:
    // header lines...
    // HEADER=END
    //  hexadecimal key
    //  hexadecimal value
    //  ... repeated
    // DATA=END

    string strLine;
    while (!strDump.eof() && strLine != HEADER_END)
        getline(strDump, strLine); // Skip past header

    std::string keyHex, valueHex;
    while (!strDump.eof() && keyHex != DATA_END) {
        getline(strDump, keyHex);
        if (keyHex != DATA_END) {
            if (strDump.eof())
                break;
            getline(strDump, valueHex);
            if (valueHex == DATA_END) {
                LogPrintf("CDBEnv::Salvage: WARNING: Number of keys in data does not match number of values.\n");
                break;
            }
            vResult.push_back(make_pair(ParseHex(keyHex), ParseHex(valueHex)));
        }
    }

    if (keyHex != DATA_END) {
        LogPrintf("CDBEnv::Salvage: WARNING: Unexpected end of file while reading salvage output.\n");
        return false;
    }

    return (result == 0);
}


void CDBEnv::CheckpointLSN(const std::string& strFile)
{
    dbenv->txn_checkpoint(0, 0, 0);
    if (fMockDb)
        return;
    dbenv->lsn_reset(strFile.c_str(), 0);
}


CDB::CDB(const fs::path& wallet_path, const char* pszMode, bool fFlushOnCloseIn) : pdb(NULL), env(NULL), activeTxn(NULL)
{
    int ret;
    fReadOnly = (!strchr(pszMode, '+') && !strchr(pszMode, 'w'));
    fFlushOnClose = fFlushOnCloseIn;
    if (wallet_path.empty())
        return;

    bool fCreate = strchr(pszMode, 'c') != NULL;
    unsigned int nFlags = DB_THREAD;
    if (fCreate)
        nFlags |= DB_CREATE;

    env = GetWalletEnv(wallet_path, strFile);

    {
        LOCK(cs_db);
        if (!env->Open(false /* retry */))
            throw runtime_error("CDB: Failed to open database environment.");

        ++env->mapFileUseCount[strFile];
        pdb = env->mapDb[strFile];
        if (pdb == NULL) {
            pdb = new Db(env->dbenv, 0);

            bool fMockDb = env->IsMock();
            if (fMockDb) {
                DbMpoolFile* mpf = pdb->get_mpf();
                ret = mpf->set_flags(DB_MPOOL_NOFILE, 1);
                if (ret != 0)
                    throw runtime_error(strprintf("CDB: Failed to configure for no temp file backing for database %s", strFile));
            }

            ret = pdb->open(NULL,                               // Txn pointer
                            fMockDb ? NULL : strFile.c_str(),   // Filename
                            fMockDb ? strFile.c_str() : "main", // Logical db name
                            DB_BTREE,                           // Database type
                            nFlags,                             // Flags
                            0);

            if (ret != 0) {
                delete pdb;
                pdb = NULL;
                --env->mapFileUseCount[strFile];
                strFile = "";
                throw runtime_error(strprintf("CDB: Error %d, can't open database %s", ret, wallet_path.string()));
            }

            if (fCreate && !Exists(string("version"))) {
                bool fTmp = fReadOnly;
                fReadOnly = false;
                WriteVersion(CLIENT_VERSION);
                fReadOnly = fTmp;
            }

            env->mapDb[strFile] = pdb;
        }
    }
}

// Legacy entry: callers who only know a relative filename get the
// "single env at GetWalletDir()" semantics they're used to. Existing
// (pre-multi-env) call sites keep working unchanged; new code should
// hand the full path to the fs::path overload above.
//
// Important: an empty strFilename must NOT be resolved via
// GetWalletDir() / "" — that returns GetWalletDir() itself and would
// make GetWalletEnv pick up the wallet-dir's last path component
// ("regtest" in tests) as the BDB filename, opening the wrong file.
// Hand an empty fs::path down so the path-based ctor's empty-check
// fires and produces a no-op CDB the way callers expect.
CDB::CDB(const std::string& strFilename, const char* pszMode, bool fFlushOnCloseIn)
    : CDB(strFilename.empty() ? fs::path() : GetWalletDir() / strFilename, pszMode, fFlushOnCloseIn)
{
}

void CDB::Flush()
{
    if (activeTxn || !env)
        return;

    // Flush database activity from memory pool to disk log
    unsigned int nMinutes = 0;
    if (fReadOnly)
        nMinutes = 1;

    env->dbenv->txn_checkpoint(nMinutes ? GetArg("-dblogsize", DEFAULT_WALLET_DBLOGSIZE) * 1024 : 0, nMinutes, 0);
}

void CDB::Close()
{
    if (!pdb)
        return;
    if (activeTxn)
        activeTxn->abort();
    activeTxn = NULL;
    pdb = NULL;

    if (fFlushOnClose)
        Flush();

    if (env) {
        LOCK(cs_db);
        --env->mapFileUseCount[strFile];
    }
}

void CDBEnv::CloseDb(const string& strFile)
{
    {
        LOCK(cs_db);
        if (mapDb[strFile] != NULL) {
            // Close the database handle
            Db* pdb = mapDb[strFile];
            pdb->close(0);
            delete pdb;
            mapDb[strFile] = NULL;
        }
    }
}

bool CDBEnv::RemoveDb(const string& strFile)
{
    this->CloseDb(strFile);

    LOCK(cs_db);
    int rc = dbenv->dbremove(NULL, strFile.c_str(), NULL, DB_AUTO_COMMIT);
    return (rc == 0);
}

bool CDB::Rewrite(const string& strFile, const char* pszSkip)
{
    // strFile is a wallet path relative to GetWalletDir() (matches the
    // legacy CDB constructor's interpretation). Resolve the env from it
    // so we operate on the right BDB environment for multi-env hosts.
    std::string dbFilename;
    CDBEnv* env = GetWalletEnv(GetWalletDir() / strFile, dbFilename);
    while (true) {
        {
            LOCK(cs_db);
            if (!env->mapFileUseCount.count(dbFilename) || env->mapFileUseCount[dbFilename] == 0) {
                // Flush log data to the dat file
                env->CloseDb(dbFilename);
                env->CheckpointLSN(dbFilename);
                env->mapFileUseCount.erase(dbFilename);

                bool fSuccess = true;
                LogPrintf("CDB::Rewrite: Rewriting %s...\n", strFile);
                string strFileRes = dbFilename + ".rewrite";
                { // surround usage of db with extra {}
                    CDB db(strFile, "r");
                    Db* pdbCopy = new Db(env->dbenv, 0);

                    int ret = pdbCopy->open(NULL,               // Txn pointer
                                            strFileRes.c_str(), // Filename
                                            "main",             // Logical db name
                                            DB_BTREE,           // Database type
                                            DB_CREATE,          // Flags
                                            0);
                    if (ret > 0) {
                        LogPrintf("CDB::Rewrite: Can't create database file %s\n", strFileRes);
                        fSuccess = false;
                    }

                    Dbc* pcursor = db.GetCursor();
                    if (pcursor)
                        while (fSuccess) {
                            CDataStream ssKey(SER_DISK, CLIENT_VERSION);
                            CDataStream ssValue(SER_DISK, CLIENT_VERSION);
                            int ret1 = db.ReadAtCursor(pcursor, ssKey, ssValue);
                            if (ret1 == DB_NOTFOUND) {
                                pcursor->close();
                                break;
                            } else if (ret1 != 0) {
                                pcursor->close();
                                fSuccess = false;
                                break;
                            }
                            if (pszSkip &&
                                strncmp(ssKey.data(), pszSkip, std::min(ssKey.size(), strlen(pszSkip))) == 0)
                                continue;
                            if (strncmp(ssKey.data(), "\x07version", 8) == 0) {
                                // Update version:
                                ssValue.clear();
                                ssValue << CLIENT_VERSION;
                            }
                            Dbt datKey(ssKey.data(), ssKey.size());
                            Dbt datValue(ssValue.data(), ssValue.size());
                            int ret2 = pdbCopy->put(NULL, &datKey, &datValue, DB_NOOVERWRITE);
                            if (ret2 > 0)
                                fSuccess = false;
                        }
                    if (fSuccess) {
                        db.Close();
                        env->CloseDb(dbFilename);
                        if (pdbCopy->close(0))
                            fSuccess = false;
                        delete pdbCopy;
                    }
                }
                if (fSuccess) {
                    Db dbA(env->dbenv, 0);
                    if (dbA.remove(dbFilename.c_str(), NULL, 0))
                        fSuccess = false;
                    Db dbB(env->dbenv, 0);
                    if (dbB.rename(strFileRes.c_str(), NULL, dbFilename.c_str(), 0))
                        fSuccess = false;
                }
                if (!fSuccess)
                    LogPrintf("CDB::Rewrite: Failed to rewrite database file %s\n", strFileRes);
                return fSuccess;
            }
        }
        MilliSleep(100);
    }
    return false;
}


void CDBEnv::Flush(bool fShutdown)
{
    int64_t nStart = GetTimeMillis();
    // Flush log data to the actual data file on all files that are not in use
    LogPrint("db", "CDBEnv::Flush: Flush(%s)%s\n", fShutdown ? "true" : "false", fDbEnvInit ? "" : " database not started");
    if (!fDbEnvInit)
        return;
    {
        LOCK(cs_db);
        map<string, int>::iterator mi = mapFileUseCount.begin();
        while (mi != mapFileUseCount.end()) {
            string strFile = (*mi).first;
            int nRefCount = (*mi).second;
            LogPrint("db", "CDBEnv::Flush: Flushing %s (refcount = %d)...\n", strFile, nRefCount);
            if (nRefCount == 0) {
                // Move log data to the dat file
                CloseDb(strFile);
                LogPrint("db", "CDBEnv::Flush: %s checkpoint\n", strFile);
                dbenv->txn_checkpoint(0, 0, 0);
                LogPrint("db", "CDBEnv::Flush: %s detach\n", strFile);
                if (!fMockDb)
                    dbenv->lsn_reset(strFile.c_str(), 0);
                LogPrint("db", "CDBEnv::Flush: %s closed\n", strFile);
                mapFileUseCount.erase(mi++);
            } else
                mi++;
        }
        LogPrint("db", "CDBEnv::Flush: Flush(%s)%s took %15dms\n", fShutdown ? "true" : "false", fDbEnvInit ? "" : " database not started", GetTimeMillis() - nStart);
        if (fShutdown) {
            char** listp;
            if (mapFileUseCount.empty()) {
                dbenv->log_archive(&listp, DB_ARCH_REMOVE);
                Close();
                if (!fMockDb)
                    fs::remove_all(fs::path(strPath) / "database");
            }
        }
    }
}
