// Copyright (c) 2021-2025 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.
#ifndef DOGECOIN_QT_IMPORTBIP39DIALOG_H
#define DOGECOIN_QT_IMPORTBIP39DIALOG_H

#include <QDialog>
#include <QThread>
#include <QVector>

class CWallet;
class CBlockIndex;
class QLineEdit;
class QCompleter;
class QStringListModel;
class PlatformStyle;

namespace Ui { class ImportBip39Dialog; }

/** Dialog for importing a BIP-39 mnemonic (12 / 24 words). */
class ImportBip39Dialog : public QDialog
{
    Q_OBJECT
public:
    explicit ImportBip39Dialog(const PlatformStyle *platformStyle,
                               const QString& cliMnemonic = "",
                               const QString& cliPassphrase = "",
                               const QString& cliKeyPath = "",
                               const QString& cliExtraWord = "",
                               QWidget *parent = nullptr);
    ~ImportBip39Dialog() override;

Q_SIGNALS:
    void stopExecutor();
    void rescanWallet(CWallet *wallet, CBlockIndex *genesis);

private Q_SLOTS:
    void setOkButtonState();
    void toggleExtraWords(bool show24);
    void toggleShowMnemonic(bool show);
    void toggleShowExtraWord(bool show);
    void onWordListChanged(int index);
    void on_resetButton_clicked();
    void on_okButton_clicked();
    void on_cancelButton_clicked();

private:
    void    loadWordList(const QString &fileAlias);
    void    setupLineEdits();
    void    rebuildCompleter();
    QString collectMnemonic()      const;
    QString extraWord()            const;
    QString walletPassphrase()     const;
    QString keyPath()              const;
    void    resetDialogValues();
    bool    importMnemonic();

    Ui::ImportBip39Dialog *ui {nullptr};
    const PlatformStyle   *style {nullptr};
    QThread                rescanThread;
    QVector<QLineEdit*>    edits;
    QCompleter            *completer {nullptr};
    QStringListModel      *wordModel {nullptr};
    QStringList            currentWords;
};

#endif // DOGECOIN_QT_IMPORTBIP39DIALOG_H
