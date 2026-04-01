// Copyright (c) 2021-2025 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef DOGECOIN_QT_IMPORTBIP39DIALOG_H
#define DOGECOIN_QT_IMPORTBIP39DIALOG_H

#include <QDialog>
#include <QThread>
#include <QVector>
#include <boost/signals2/connection.hpp>
#include <string>

class CWallet;
class CBlockIndex;
class QLineEdit;
class QCompleter;
class QStringListModel;
class PlatformStyle;

namespace Ui { class ImportBip39Dialog; }

/** Dialog for importing a BIP-39 mnemonic (12 / 24 words) + sweep. */
class ImportBip39Dialog : public QDialog
{
    Q_OBJECT
public:
    explicit ImportBip39Dialog(const PlatformStyle *platformStyle, QWidget *parent = nullptr);
    ~ImportBip39Dialog() override;

Q_SIGNALS:
    void stopExecutor();
    void rescanWallet(CWallet *wallet, CBlockIndex *start);

private Q_SLOTS:
    void setOkButtonState();
    void toggleExtraWords(bool show24);
    void toggleShowMnemonic(bool show);
    void toggleShowExtraWord(bool show);
    void onWordListChanged(int);
    void on_resetButton_clicked();
    void on_okButton_clicked();
    void on_cancelButton_clicked();
    void on_sweepButton_clicked();
    void updateRescanProgress(const QString& title, int progress);

private:
    void    loadWordList(const QString &fileAlias);
    void    setupLineEdits();
    void    rebuildCompleter();
    QString collectMnemonic()      const;
    QString extraWord()            const;
    QString walletPassphrase()     const;
    QString keyPath()              const;
    QString sweepDestination()     const;
    void    resetDialogValues();
    bool    importMnemonic();

    CBlockIndex* rescanStartIndex() const;

    void attachProgress();
    void detachProgress();

    void startSweepWorker(bool dryRun);
    void handleSweepFinished(bool ok, const QString& err, int inputs, qint64 total, qint64 fee,
                             const QString& sent_to, const QString& txid, bool dryRun);

    void setBusy(bool busy, bool indeterminate, const QString& status);

    void updateModeUi(); // show/hide import vs sweep elements

    Ui::ImportBip39Dialog *ui {nullptr};
    const PlatformStyle   *style {nullptr};
    QThread                rescanThread;
    QThread                sweepThread_;
    QVector<QLineEdit*>    edits;
    QCompleter            *completer {nullptr};
    QStringListModel      *wordModel {nullptr};
    QStringList            currentWords;

    std::string           cached_mnemonic_;
    std::string           cached_extra_;
    std::string           cached_basepath_;
    std::string           cached_dest_;
    int                   cached_gap_{20};
    CBlockIndex*          cached_start_{nullptr};

    bool                  import_mode_{false};

    boost::signals2::scoped_connection progress_conn_;
};

#endif // DOGECOIN_QT_IMPORTBIP39DIALOG_H
