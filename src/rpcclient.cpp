// Copyright (c) 2010 Satoshi Nakamoto
// Copyright (c) 2009-2013 The Bitcoin developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "rpcclient.h"

#include "rpcprotocol.h"
#include "util.h"
#include "ui_interface.h"
#include "chainparams.h" // for Params().RPCPort()

#include <stdint.h>

#include <boost/algorithm/string.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/bind.hpp>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>
#include <boost/iostreams/concepts.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/shared_ptr.hpp>
#include "json/json_spirit_writer_template.h"

using namespace std;
using namespace boost;
using namespace boost::asio;
using namespace json_spirit;

Object CallRPC(const string& strMethod, const Array& params)
{
    if (mapArgs["-rpcuser"] == "" && mapArgs["-rpcpassword"] == "")
        throw runtime_error(strprintf(
            _("You must set rpcpassword=<password> in the configuration file:\n%s\n"
              "If the file does not exist, create it with owner-readable-only file permissions."),
                GetConfigFile().string().c_str()));

static const CRPCConvertParam vRPCConvertParams[] =
{
    { "stop", 0 },
    { "getaddednodeinfo", 0 },
    { "setgenerate", 0 },
    { "setgenerate", 1 },
    { "getnetworkhashps", 0 },
    { "getnetworkhashps", 1 },
    { "sendtoaddress", 1 },
    { "settxfee", 0 },
    { "getreceivedbyaddress", 1 },
    { "getreceivedbyaccount", 1 },
    { "listreceivedbyaddress", 0 },
    { "listreceivedbyaddress", 1 },
    { "listreceivedbyaddress", 2 },
    { "listreceivedbyaccount", 0 },
    { "listreceivedbyaccount", 1 },
    { "listreceivedbyaccount", 2 },
    { "getbalance", 1 },
    { "getbalance", 2 },
    { "getblockhash", 0 },
    { "move", 2 },
    { "move", 3 },
    { "sendfrom", 2 },
    { "sendfrom", 3 },
    { "listtransactions", 1 },
    { "listtransactions", 2 },
    { "listtransactions", 3 },
    { "listaccounts", 0 },
    { "listaccounts", 1 },
    { "walletpassphrase", 1 },
    { "getblocktemplate", 0 },
    { "listsinceblock", 1 },
    { "listsinceblock", 2 },
    { "sendmany", 1 },
    { "sendmany", 2 },
    { "addmultisigaddress", 0 },
    { "addmultisigaddress", 1 },
    { "createmultisig", 0 },
    { "createmultisig", 1 },
    { "listunspent", 0 },
    { "listunspent", 1 },
    { "listunspent", 2 },
    { "getblock", 1 },
    { "getrawtransaction", 1 },
    { "createrawtransaction", 0 },
    { "createrawtransaction", 1 },
    { "signrawtransaction", 1 },
    { "signrawtransaction", 2 },
    { "sendrawtransaction", 1 },
    { "gettxout", 1 },
    { "gettxout", 2 },
    { "lockunspent", 0 },
    { "lockunspent", 1 },
    { "importprivkey", 2 },
    { "importaddress", 2 },
    { "verifychain", 0 },
    { "verifychain", 1 },
    { "keypoolrefill", 0 },
    { "getrawmempool", 0 },
    { "estimatefee", 0 },
    { "estimatepriority", 0 },
};

class CRPCConvertTable
{
private:
    std::set<std::pair<std::string, int> > members;

    bool fWait = GetBoolArg("-rpcwait", false); // -rpcwait means try until server has started
    do {
        bool fConnected = d.connect(GetArg("-rpcconnect", "127.0.0.1"), GetArg("-rpcport", itostr(Params().RPCPort())));
        if (fConnected) break;
        if (fWait)
            MilliSleep(1000);
        else
            throw runtime_error("couldn't connect to server");
    } while (fWait);

    // HTTP basic authentication
    string strUserPass64 = EncodeBase64(mapArgs["-rpcuser"] + ":" + mapArgs["-rpcpassword"]);
    map<string, string> mapRequestHeaders;
    mapRequestHeaders["Authorization"] = string("Basic ") + strUserPass64;

    // Send request
    string strRequest = JSONRPCRequest(strMethod, params, 1);
    string strPost = HTTPPost(strRequest, mapRequestHeaders);
    stream << strPost << std::flush;

    // Receive HTTP reply status
    int nProto = 0;
    int nStatus = ReadHTTPStatus(stream, nProto);

    // Receive HTTP reply message headers and body
    map<string, string> mapHeaders;
    string strReply;
    ReadHTTPMessage(stream, mapHeaders, strReply, nProto);

    if (nStatus == HTTP_UNAUTHORIZED)
        throw runtime_error("incorrect rpcuser or rpcpassword (authorization failed)");
    else if (nStatus >= 400 && nStatus != HTTP_BAD_REQUEST && nStatus != HTTP_NOT_FOUND && nStatus != HTTP_INTERNAL_SERVER_ERROR)
        throw runtime_error(strprintf("server returned HTTP error %d", nStatus));
    else if (strReply.empty())
        throw runtime_error("no response from server");

    // Parse reply
    Value valReply;
    if (!read_string(strReply, valReply))
        throw runtime_error("couldn't parse reply from server");
    const Object& reply = valReply.get_obj();
    if (reply.empty())
        throw runtime_error("expected reply to have result, error and id properties");

    return reply;
}

template<typename T>
void ConvertTo(Value& value, bool fAllowNull=false)
{
    if (fAllowNull && value.type() == null_type)
        return;
    if (value.type() == str_type)
    {
        // reinterpret string as unquoted json value
        Value value2;
        string strJSON = value.get_str();
        if (!read_string(strJSON, value2))
            throw runtime_error(string("Error parsing JSON:")+strJSON);
        ConvertTo<T>(value2, fAllowNull);
        value = value2;
    }
    else
    {
        value = value.get_value<T>();
    }
}

// Convert strings to command-specific RPC representation
Array RPCConvertValues(const std::string &strMethod, const std::vector<std::string> &strParams)
{
    Array params;
    BOOST_FOREACH(const std::string &param, strParams)
        params.push_back(param);

    int n = params.size();

    //
    // Special case non-string parameter types
    //
    if (strMethod == "stop"                   && n > 0) ConvertTo<bool>(params[0]);
    if (strMethod == "getaddednodeinfo"       && n > 0) ConvertTo<bool>(params[0]);
    if (strMethod == "setgenerate"            && n > 0) ConvertTo<bool>(params[0]);
    if (strMethod == "setgenerate"            && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "getnetworkhashps"       && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "getnetworkhashps"       && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "sendtoaddress"          && n > 1) ConvertTo<double>(params[1]);
    if (strMethod == "settxfee"               && n > 0) ConvertTo<double>(params[0]);
    if (strMethod == "getreceivedbyaddress"   && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "getreceivedbyaccount"   && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "listreceivedbyaddress"  && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "listreceivedbyaddress"  && n > 1) ConvertTo<bool>(params[1]);
    if (strMethod == "listreceivedbyaccount"  && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "listreceivedbyaccount"  && n > 1) ConvertTo<bool>(params[1]);
    if (strMethod == "getbalance"             && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "getblockhash"           && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "move"                   && n > 2) ConvertTo<double>(params[2]);
    if (strMethod == "move"                   && n > 3) ConvertTo<int64_t>(params[3]);
    if (strMethod == "sendfrom"               && n > 2) ConvertTo<double>(params[2]);
    if (strMethod == "sendfrom"               && n > 3) ConvertTo<int64_t>(params[3]);
    if (strMethod == "listtransactions"       && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "listtransactions"       && n > 2) ConvertTo<int64_t>(params[2]);
    if (strMethod == "listaccounts"           && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "walletpassphrase"       && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "getblocktemplate"       && n > 0) ConvertTo<Object>(params[0]);
    if (strMethod == "listsinceblock"         && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "sendmany"               && n > 1) ConvertTo<Object>(params[1]);
    if (strMethod == "sendmany"               && n > 2) ConvertTo<int64_t>(params[2]);
    if (strMethod == "addmultisigaddress"     && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "addmultisigaddress"     && n > 1) ConvertTo<Array>(params[1]);
    if (strMethod == "createmultisig"         && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "createmultisig"         && n > 1) ConvertTo<Array>(params[1]);
    if (strMethod == "listunspent"            && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "listunspent"            && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "listunspent"            && n > 2) ConvertTo<Array>(params[2]);
    if (strMethod == "getblock"               && n > 1) ConvertTo<bool>(params[1]);
    if (strMethod == "getrawtransaction"      && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "createrawtransaction"   && n > 0) ConvertTo<Array>(params[0]);
    if (strMethod == "createrawtransaction"   && n > 1) ConvertTo<Object>(params[1]);
    if (strMethod == "signrawtransaction"     && n > 1) ConvertTo<Array>(params[1], true);
    if (strMethod == "signrawtransaction"     && n > 2) ConvertTo<Array>(params[2], true);
    if (strMethod == "sendrawtransaction"     && n > 1) ConvertTo<bool>(params[1], true);
    if (strMethod == "gettxout"               && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "gettxout"               && n > 2) ConvertTo<bool>(params[2]);
    if (strMethod == "lockunspent"            && n > 0) ConvertTo<bool>(params[0]);
    if (strMethod == "lockunspent"            && n > 1) ConvertTo<Array>(params[1]);
    if (strMethod == "importprivkey"          && n > 2) ConvertTo<bool>(params[2]);
    if (strMethod == "verifychain"            && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "verifychain"            && n > 1) ConvertTo<int64_t>(params[1]);
    if (strMethod == "keypoolrefill"          && n > 0) ConvertTo<int64_t>(params[0]);
    if (strMethod == "getrawmempool"          && n > 0) ConvertTo<bool>(params[0]);

    return params;
}

int CommandLineRPC(int argc, char *argv[])
{
    string strPrint;
    int nRet = 0;
    try
    {
        // Skip switches
        while (argc > 1 && IsSwitchChar(argv[1][0]))
        {
            argc--;
            argv++;
        }

        // Method
        if (argc < 2)
            throw runtime_error("too few parameters");
        string strMethod = argv[1];

        // Parameters default to strings
        std::vector<std::string> strParams(&argv[2], &argv[argc]);
        Array params = RPCConvertValues(strMethod, strParams);

        // Execute
        Object reply = CallRPC(strMethod, params);

        // Parse reply
        const Value& result = find_value(reply, "result");
        const Value& error  = find_value(reply, "error");

        if (error.type() != null_type)
        {
            // Error
            strPrint = "error: " + write_string(error, false);
            int code = find_value(error.get_obj(), "code").get_int();
            nRet = abs(code);
        }
        else
        {
            // Result
            if (result.type() == null_type)
                strPrint = "";
            else if (result.type() == str_type)
                strPrint = result.get_str();
            else
                strPrint = write_string(result, true);
        }
    }
    return nRet;
}

std::string HelpMessageCli(bool mainProgram)
{
    string strUsage;
    if(mainProgram)
    {
        strUsage += _("Options:") + "\n";
        strUsage += "  -?                     " + _("This help message") + "\n";
        strUsage += "  -conf=<file>           " + _("Specify configuration file (default: dogecoin.conf)") + "\n";
        strUsage += "  -datadir=<dir>         " + _("Specify data directory") + "\n";
        strUsage += "  -testnet               " + _("Use the test network") + "\n";
        strUsage += "  -regtest               " + _("Enter regression test mode, which uses a special chain in which blocks can be "
                                                    "solved instantly. This is intended for regression testing tools and app development.") + "\n";
    } else {
        strUsage += _("RPC client options:") + "\n";
    }

    strUsage += "  -rpcconnect=<ip>       " + _("Send commands to node running on <ip> (default: 127.0.0.1)") + "\n";
    strUsage += "  -rpcport=<port>        " + _("Connect to JSON-RPC on <port> (default: 22555 or testnet: 44555)") + "\n";
    strUsage += "  -rpcwait               " + _("Wait for RPC server to start") + "\n";

    if(mainProgram)
    {
        strUsage += "  -rpcuser=<user>        " + _("Username for JSON-RPC connections") + "\n";
        strUsage += "  -rpcpassword=<pw>      " + _("Password for JSON-RPC connections") + "\n";

        strUsage += "\n" + _("SSL options: (see the Bitcoin Wiki for SSL setup instructions)") + "\n";
        strUsage += "  -rpcssl                " + _("Use OpenSSL (https) for JSON-RPC connections") + "\n";
    }

    return strUsage;
}

