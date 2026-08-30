// Copyright (c) 2009-2016 The Bitcoin Core developers
// Copyright (c) 2021-2023 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.
//
// RPC interface to the local index database (db/schema.sql). Read-only:
// this file never writes to the index DB, it just exposes it over RPC so
// external tooling (the Node/Python install+watch scripts under db/) can
// own ingestion while node operators can query index state the same way
// they query any other RPC method.

#include "rpc/server.h"
#include "utilstrencodings.h"
#include "util.h"

#include <stdexcept>
#include <string>

#include <univalue.h>
#include <sqlite3.h>

// Path to the index database. Overridable with -indexdb=<path>; defaults to
// <datadir>/dogeindex.sqlite3, mirroring how wallet.dat is located relative
// to datadir elsewhere in this codebase.
static std::string IndexDbPath()
{
    return GetArg("-indexdb", (GetDataDir() / "dogeindex.sqlite3").string());
}

// RAII wrapper so early "throw runtime_error()" paths (matching this repo's
// existing RPC error-handling style) can't leak the sqlite3* handle.
class CIndexDbHandle
{
public:
    CIndexDbHandle()
    {
        int rc = sqlite3_open_v2(IndexDbPath().c_str(), &db, SQLITE_OPEN_READONLY, nullptr);
        if (rc != SQLITE_OK) {
            std::string err = db ? sqlite3_errmsg(db) : "unknown error";
            if (db) sqlite3_close(db);
            db = nullptr;
            throw std::runtime_error(
                "Could not open index database at " + IndexDbPath() + ": " + err +
                ". Run db/install.py or db/install.js first.");
        }
    }
    ~CIndexDbHandle()
    {
        if (db) sqlite3_close(db);
    }
    sqlite3* get() const { return db; }

private:
    sqlite3* db = nullptr;
};

UniValue getindexstats(const JSONRPCRequest& request)
{
    if (request.fHelp || request.params.size() != 0)
        throw std::runtime_error(
            "getindexstats\n"
            "\nReturns summary statistics from the local RPC index database\n"
            "(populated via db/install.py, db/install.js, or db/install.sh).\n"
            "\nResult:\n"
            "{\n"
            "  \"schema_version\": xxxxx,      (numeric) index schema version\n"
            "  \"synced_height\": xxxxx,       (numeric) last block height ingested into the index\n"
            "  \"blocks_indexed\": xxxxx,      (numeric) rows in blocks_index\n"
            "  \"tx_indexed\": xxxxx,          (numeric) rows in tx_index\n"
            "  \"external_refs\": xxxxx,       (numeric) rows in external_chain_refs\n"
            "  \"external_refs_by_chain\": {   (object) external_refs grouped by chain\n"
            "    \"pi-network\": xxxxx,\n"
            "    \"stellar-soroban\": xxxxx,\n"
            "    \"solana\": xxxxx,\n"
            "    \"ethereum\": xxxxx\n"
            "  }\n"
            "}\n"
            "\nExamples:\n"
            + HelpExampleCli("getindexstats", "")
            + HelpExampleRpc("getindexstats", "")
        );

    CIndexDbHandle handle;
    sqlite3* db = handle.get();
    UniValue result(UniValue::VOBJ);

    {
        sqlite3_stmt* stmt = nullptr;
        const char* sql = "SELECT schema_version, synced_height FROM index_meta WHERE id = 1;";
        if (sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr) == SQLITE_OK && sqlite3_step(stmt) == SQLITE_ROW) {
            result.pushKV("schema_version", sqlite3_column_int(stmt, 0));
            result.pushKV("synced_height", sqlite3_column_int64(stmt, 1));
        } else {
            result.pushKV("schema_version", UniValue::VNULL);
            result.pushKV("synced_height", UniValue::VNULL);
        }
        if (stmt) sqlite3_finalize(stmt);
    }

    auto countRows = [&](const char* table) -> int64_t {
        sqlite3_stmt* stmt = nullptr;
        std::string sql = std::string("SELECT COUNT(*) FROM ") + table + ";";
        int64_t n = 0;
        if (sqlite3_prepare_v2(db, sql.c_str(), -1, &stmt, nullptr) == SQLITE_OK && sqlite3_step(stmt) == SQLITE_ROW) {
            n = sqlite3_column_int64(stmt, 0);
        }
        if (stmt) sqlite3_finalize(stmt);
        return n;
    };

    result.pushKV("blocks_indexed", countRows("blocks_index"));
    result.pushKV("tx_indexed", countRows("tx_index"));
    result.pushKV("external_refs", countRows("external_chain_refs"));

    UniValue byChain(UniValue::VOBJ);
    {
        sqlite3_stmt* stmt = nullptr;
        const char* sql = "SELECT chain, COUNT(*) FROM external_chain_refs GROUP BY chain;";
        if (sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr) == SQLITE_OK) {
            while (sqlite3_step(stmt) == SQLITE_ROW) {
                const char* chain = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
                int64_t n = sqlite3_column_int64(stmt, 1);
                if (chain) byChain.pushKV(chain, n);
            }
        }
        if (stmt) sqlite3_finalize(stmt);
    }
    result.pushKV("external_refs_by_chain", byChain);

    return result;
}

UniValue getindexrecord(const JSONRPCRequest& request)
{
    if (request.fHelp || request.params.size() != 1)
        throw std::runtime_error(
            "getindexrecord \"txid\"\n"
            "\nLooks up a transaction in the local index database and\n"
            "returns its indexed summary plus any linked external chain refs\n"
            "(Pi Network, Stellar/Soroban, Solana, Ethereum).\n"
            "\nArguments:\n"
            "1. \"txid\"    (string, required) the transaction id\n"
            "\nResult:\n"
            "{\n"
            "  \"txid\": \"xxxx\",\n"
            "  \"block_height\": xxxxx,\n"
            "  \"value_out\": xxxxx,\n"
            "  \"fee\": xxxxx,\n"
            "  \"vin_count\": xxxxx,\n"
            "  \"vout_count\": xxxxx,\n"
            "  \"external_refs\": [\n"
            "    { \"chain\": \"xxxx\", \"external_ref\": \"xxxx\", \"ref_type\": \"xxxx\" }, ...\n"
            "  ]\n"
            "}\n"
            "\nExamples:\n"
            + HelpExampleCli("getindexrecord", "\"a1b2c3...\"")
            + HelpExampleRpc("getindexrecord", "\"a1b2c3...\"")
        );

    std::string txid = request.params[0].get_str();

    CIndexDbHandle handle;
    sqlite3* db = handle.get();
    UniValue result(UniValue::VOBJ);
    bool found = false;

    {
        sqlite3_stmt* stmt = nullptr;
        const char* sql =
            "SELECT txid, block_height, value_out, fee, vin_count, vout_count "
            "FROM tx_index WHERE txid = ?1;";
        if (sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr) == SQLITE_OK) {
            sqlite3_bind_text(stmt, 1, txid.c_str(), -1, SQLITE_TRANSIENT);
            if (sqlite3_step(stmt) == SQLITE_ROW) {
                found = true;
                result.pushKV("txid", reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0)));
                result.pushKV("block_height", sqlite3_column_int64(stmt, 1));
                result.pushKV("value_out", sqlite3_column_double(stmt, 2));
                if (sqlite3_column_type(stmt, 3) != SQLITE_NULL)
                    result.pushKV("fee", sqlite3_column_double(stmt, 3));
                else
                    result.pushKV("fee", UniValue::VNULL);
                result.pushKV("vin_count", sqlite3_column_int(stmt, 4));
                result.pushKV("vout_count", sqlite3_column_int(stmt, 5));
            }
        }
        if (stmt) sqlite3_finalize(stmt);
    }

    if (!found)
        throw std::runtime_error("txid not found in index database: " + txid);

    UniValue refs(UniValue::VARR);
    {
        sqlite3_stmt* stmt = nullptr;
        const char* sql =
            "SELECT chain, external_ref, ref_type FROM external_chain_refs "
            "WHERE doge_txid = ?1 ORDER BY created_at ASC;";
        if (sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr) == SQLITE_OK) {
            sqlite3_bind_text(stmt, 1, txid.c_str(), -1, SQLITE_TRANSIENT);
            while (sqlite3_step(stmt) == SQLITE_ROW) {
                UniValue ref(UniValue::VOBJ);
                ref.pushKV("chain", reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0)));
                ref.pushKV("external_ref", reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1)));
                ref.pushKV("ref_type", reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2)));
                refs.push_back(ref);
            }
        }
        if (stmt) sqlite3_finalize(stmt);
    }
    result.pushKV("external_refs", refs);

    return result;
}

static const CRPCCommand commands[] =
{ //  category      name                actor (function)      okSafeMode
  //  ------------- ------------------  ---------------------- ----------
    { "indexing",   "getindexstats",    &getindexstats,        true,  {} },
    { "indexing",   "getindexrecord",   &getindexrecord,       true,  {"txid"} },
};

void RegisterIndexRPCCommands(CRPCTable &t)
{
    for (unsigned int vcidx = 0; vcidx < ARRAYLEN(commands); vcidx++)
        t.appendCommand(commands[vcidx].name, &commands[vcidx]);
}
