// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_QT_TEST_OPTIONTESTS_H
#define BITCOIN_QT_TEST_OPTIONTESTS_H

#include <QObject>
#include <QTest>

class OptionTests : public QObject
{
    Q_OBJECT

private Q_SLOTS:
    void init();
    void cleanup();

    void autoAddSendAddressesDefault();
    void autoAddSendAddressesPersists();
};

#endif // BITCOIN_QT_TEST_OPTIONTESTS_H
