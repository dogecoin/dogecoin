#ifndef WALLETBAKEUPTESTS_H
#define WALLETBAKEUPTESTS_H


#include <QObject>
#include <QTest>

 /* Feature 1 - Testing Automatkup functionalities*/

class WalletBackupTests : public QObject
{
    Q_OBJECT

private slots:
 /* Feature 1 - Testing Bakup on startup if this option is enabled*/
    void startupBackupTests();
};


 #endif // WALLETBAKEUPTESTS_H
