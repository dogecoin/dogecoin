// Copyright (c) 2016 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "wallet/test/wallet_test_fixture.h"

#include "rpc/server.h"
#include "wallet/db.h"
#include "wallet/wallet.h"

WalletTestingSetup::WalletTestingSetup(const std::string& chainName):
    TestingSetup(chainName)
{
    // Resolve the env for the test wallet's BDB-relative filename and
    // put THAT env into mock mode. In the multi-env world there is no
    // single global env to mock; the test wallet lives in an env at
    // GetWalletDir() so that's the one we mock.
    std::string dbFilename;
    CDBEnv* testEnv = GetWalletEnv(GetWalletDir() / "wallet_test.dat", dbFilename);
    testEnv->MakeMock();

    bool fFirstRun;
    pwalletMain = new CWallet("wallet_test.dat");
    pwalletMain->LoadWallet(fFirstRun);
    RegisterValidationInterface(pwalletMain);
    vpwallets.push_back(pwalletMain);

    RegisterWalletRPCCommands(tableRPC);
}

WalletTestingSetup::~WalletTestingSetup()
{
    UnregisterValidationInterface(pwalletMain);
    vpwallets.erase(std::remove(vpwallets.begin(), vpwallets.end(), pwalletMain), vpwallets.end());
    delete pwalletMain;
    pwalletMain = NULL;

    std::string dbFilename;
    CDBEnv* testEnv = GetWalletEnv(GetWalletDir() / "wallet_test.dat", dbFilename);
    testEnv->Flush(true);
    testEnv->Reset();
}
