// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "optiontests.h"

#include "optionsmodel.h"

#include <QSettings>
#include <QSignalSpy>

static const QString AUTO_ADD_KEY = "fAutoAddSendAddresses";

// QSettings is process-wide and outlives the test binary, so start each test
// from a known state rather than from whatever the last run left behind.
void OptionTests::init()
{
    QSettings().remove(AUTO_ADD_KEY);
}

void OptionTests::cleanup()
{
    QSettings().remove(AUTO_ADD_KEY);
}

void OptionTests::autoAddSendAddressesDefault()
{
    // A wallet upgrading from a release without this option has no stored
    // value. Sending to an address has always stored it in the address book,
    // so the default has to keep that wallet behaving exactly as it did.
    OptionsModel options;

    QVERIFY(options.getAutoAddSendAddresses());
    QCOMPARE(options.data(options.index(OptionsModel::AutoAddSendAddresses, 0), Qt::EditRole).toBool(), true);
}

void OptionTests::autoAddSendAddressesPersists()
{
    {
        OptionsModel options;
        QSignalSpy spy(&options, SIGNAL(autoAddSendAddressesChanged(bool)));

        QVERIFY(options.setData(options.index(OptionsModel::AutoAddSendAddresses, 0), false, Qt::EditRole));
        QCOMPARE(spy.count(), 1);
        QCOMPARE(spy.takeFirst().at(0).toBool(), false);
        QVERIFY(!options.getAutoAddSendAddresses());
        QCOMPARE(QSettings().value(AUTO_ADD_KEY).toBool(), false);
    }

    // A model built afterwards has to see the stored choice rather than the
    // default, or the setting would silently revert on the next start.
    OptionsModel reloaded;
    QVERIFY(!reloaded.getAutoAddSendAddresses());
}
