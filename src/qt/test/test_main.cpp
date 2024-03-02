// Copyright (c) 2009-2016 The Bitcoin Core developers
// Copyright (c) 2021-2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#if defined(HAVE_CONFIG_H)
#include "config/bitcoin-config.h"
#endif

#include "chainparams.h"
#include "key.h"
#include "rpcnestedtests.h"
#include "util.h"
#include "uritests.h"

#include <QCoreApplication>
#include <QObject>
#include <QTest>

extern void noui_connect();

static int qt_argc = 1;
static const char* qt_argv = "dogecoin-qt";

// This is all you need to run all the tests
int main(int argc, char *argv[])
{
    ECC_Start();
    SetupEnvironment();
    SetupNetworking();
    SelectParams(CBaseChainParams::MAIN);
    noui_connect();

    bool fInvalid = false;

    // Don't remove this, it's needed to access
    // QCoreApplication:: in the tests
    QCoreApplication app(qt_argc, const_cast<char **>(&qt_argv));
    app.setApplicationName("Bitcoin-Qt-test");

    URITests test1;
    if (QTest::qExec(&test1) != 0)
        fInvalid = true;
    RPCNestedTests test3;
    if (QTest::qExec(&test3) != 0)
        fInvalid = true;

    ECC_Stop();
    return fInvalid;
}
