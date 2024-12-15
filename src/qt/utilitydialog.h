// Copyright (c) 2011-2016 The Bitcoin Core developers
// Copyright (c) 2018-2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_QT_UTILITYDIALOG_H
#define BITCOIN_QT_UTILITYDIALOG_H

#include <QDialog>
#include <QObject>
#include "walletmodel.h"

class BitcoinGUI;
class ClientModel;

namespace Ui {
    class HelpMessageDialog;
    class PaperWalletDialog;
}

/** "Paper Wallet" dialog box */
class PaperWalletDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PaperWalletDialog(QWidget *parent);
    ~PaperWalletDialog();

    void setClientModel(ClientModel *clientModel);
    void setModel(WalletModel *model);

private:
    Ui::PaperWalletDialog *ui;
    ClientModel *clientModel;
    WalletModel *model;
    static const int PAPER_WALLET_READJUST_LIMIT = 20;
    static const int PAPER_WALLET_PAGE_MARGIN = 50;

private Q_SLOTS:
    void on_getNewAddress_clicked();
    void on_printButton_clicked();
};

/** "Help message" dialog box */
class HelpMessageDialog : public QDialog
{
    Q_OBJECT

public:
    explicit HelpMessageDialog(QWidget *parent, bool about);
    ~HelpMessageDialog();

    void printToConsole();
    void showOrPrint();

private:
    Ui::HelpMessageDialog *ui;
    QString text;

private Q_SLOTS:
    void on_okButton_accepted();
};


/** "Shutdown" window */
class ShutdownWindow : public QWidget
{
    Q_OBJECT

public:
    ShutdownWindow(QWidget *parent=0, Qt::WindowFlags f=0);
    static QWidget *showShutdownWindow(BitcoinGUI *window);

protected:
    void closeEvent(QCloseEvent *event);
};

#endif // BITCOIN_QT_UTILITYDIALOG_H
