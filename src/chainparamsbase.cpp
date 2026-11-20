// Copyright (c) 2010 Satoshi Nakamoto
// Copyright (c) 2009-2015 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "chainparamsbase.h"

#include "tinyformat.h"
#include "util.h"

#include <assert.h>

const std::string CBaseChainParams::MAIN = "main";
const std::string CBaseChainParams::TESTNET = "test";
const std::string CBaseChainParams::TESTNET4 = "testnet4";
const std::string CBaseChainParams::REGTEST = "regtest";

void AppendParamsHelpMessages(std::string& strUsage, bool debugHelp)
{
    strUsage += HelpMessageGroup(_("Chain selection options:"));
    strUsage += HelpMessageOpt("-testnet", _("Use the legacy testnet3 chain"));
    strUsage += HelpMessageOpt("-testnet4", _("Use the rebooted testnet (fresh chain, T addresses, strict min-difficulty)"));
    if (debugHelp) {
        strUsage += HelpMessageOpt("-regtest", "Enter regression test mode, which uses a special chain in which blocks can be solved instantly. "
                                   "This is intended for regression testing tools and app development.");
    }
}

/**
 * Main network
 */
class CBaseMainParams : public CBaseChainParams
{
public:
    CBaseMainParams()
    {
        nRPCPort = 22555;
    }
};
static CBaseMainParams mainParams;

/**
 * Testnet (v3, legacy)
 */
class CBaseTestNetParams : public CBaseChainParams
{
public:
    CBaseTestNetParams()
    {
        nRPCPort = 44555;
        strDataDir = "testnet3";
    }
};
static CBaseTestNetParams testNetParams;

/**
 * Testnet4 (rebooted chain: new genesis / magic; same RPC port as legacy testnet3)
 */
class CBaseTestNet4Params : public CBaseChainParams
{
public:
    CBaseTestNet4Params()
    {
        nRPCPort = 44555;
        strDataDir = "testnet4";
    }
};
static CBaseTestNet4Params testNet4Params;

/*
 * Regression test
 */
class CBaseRegTestParams : public CBaseChainParams
{
public:
    CBaseRegTestParams()
    {
        nRPCPort = 18332;
        strDataDir = "regtest";
    }
};
static CBaseRegTestParams regTestParams;

static CBaseChainParams* pCurrentBaseParams = 0;

const CBaseChainParams& BaseParams()
{
    assert(pCurrentBaseParams);
    return *pCurrentBaseParams;
}

CBaseChainParams& BaseParams(const std::string& chain)
{
    if (chain == CBaseChainParams::MAIN)
        return mainParams;
    else if (chain == CBaseChainParams::TESTNET)
        return testNetParams;
    else if (chain == CBaseChainParams::TESTNET4)
        return testNet4Params;
    else if (chain == CBaseChainParams::REGTEST)
        return regTestParams;
    else
        throw std::runtime_error(strprintf("%s: Unknown chain %s.", __func__, chain));
}

void SelectBaseParams(const std::string& chain)
{
    pCurrentBaseParams = &BaseParams(chain);
}

std::string ChainNameFromCommandLine()
{
    bool fRegTest = GetBoolArg("-regtest", false);
    bool fTestNet = GetBoolArg("-testnet", false);
    bool fTestNet4 = GetBoolArg("-testnet4", false);

    if ((fTestNet && fRegTest) || (fTestNet4 && fRegTest) || (fTestNet && fTestNet4))
        throw std::runtime_error("Invalid combination of -regtest, -testnet, and -testnet4.");
    if (fRegTest)
        return CBaseChainParams::REGTEST;
    if (fTestNet4)
        return CBaseChainParams::TESTNET4;
    if (fTestNet)
        return CBaseChainParams::TESTNET;
    return CBaseChainParams::MAIN;
}

bool AreBaseParamsConfigured()
{
    return pCurrentBaseParams != NULL;
}
