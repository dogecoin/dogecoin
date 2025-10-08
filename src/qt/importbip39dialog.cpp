// Copyright (c) 2021-2025 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#if defined(HAVE_CONFIG_H)
#include "config/bitcoin-config.h"
#endif

#include "importbip39dialog.h"
#include "ui_importbip39dialog.h"

#include "platformstyle.h"
#include "wallet/bip39/wordlists/index.h"
#include "wallet/wallet.h"
#include "validation.h"
#include "chain.h"
#include "utiltime.h"
#include "utilmoneystr.h"
#include "ui_interface.h"
#include "base58.h"
#include "script/standard.h"
#include "util.h"

#include <QCompleter>
#include <QFile>
#include <QLineEdit>
#include <QThread>
#include <QTimer>
#include <QStringListModel>
#include <QMessageBox>
#include <QDateTime>
#include <QDir>
#include <QObject>

#ifdef USE_BIP39
#include "support/experimental.h"
#define LANG_WORD_CNT 2048
static const int BIP39_ADDR_GAP = 20;
EXPERIMENTAL_FEATURE

class ImportBip39Executor : public QObject
{
    Q_OBJECT
public Q_SLOTS:
    void rescan(CWallet *wallet, CBlockIndex *start)
    {
        wallet->ScanForWalletTransactions(start, true);
        QThread::currentThread()->quit();
    }
};

// Sweep worker runs in a background thread
class SweepWorker : public QObject
{
    Q_OBJECT
public:
    CWallet* wallet{nullptr};
    std::string mnemonic, extra, basePath;
    int gap{20};
    bool dry_run{true};
    std::string dest_str;          // optional sweep destination (string)
    CBlockIndex* startAt{nullptr};

Q_SIGNALS:
    void finished(bool ok, QString err, int inputs, qint64 total, qint64 fee,
                  QString sent_to, QString txid);

public Q_SLOTS:
    void run()
    {
        SweepReport rep;
        std::string err;
        const CFeeRate* fee_override = nullptr;

        // Build destination (optional)
        CTxDestination dest;
        const CTxDestination* destOpt = nullptr;
        if (!dest_str.empty()) {
            CBitcoinAddress addr(dest_str);
            if (!addr.IsValid()) {
                Q_EMIT finished(false,
                                QString("Invalid destination address."),
                                0, 0, 0, QString(), QString());
                QThread::currentThread()->quit();
                return;
            }
            dest = addr.Get();
            destOpt = &dest;
        }

        bool ok = wallet->SweepFromMnemonic(mnemonic, extra, basePath, gap,
                                            destOpt, startAt, dry_run,
                                            fee_override, rep, err);
        Q_EMIT finished(ok,
                        QString::fromStdString(err),
                        rep.inputs,
                        static_cast<qint64>(rep.total),
                        static_cast<qint64>(rep.fee),
                        QString::fromStdString(rep.sent_to),
                        QString::fromStdString(rep.txid));
        QThread::currentThread()->quit();
    }
};

#include "importbip39dialog.moc"

ImportBip39Dialog::ImportBip39Dialog(const PlatformStyle *platformStyle, QWidget *parent)
    : QDialog(parent), ui(new Ui::ImportBip39Dialog), style(platformStyle)
{
    ui->setupUi(this);

    // Decide mode based on -bip39mnemonic:
    //  - set     => Import-only mode (first-run / advanced)
    //  - not set => Sweep-only mode (default)
    import_mode_ = GetBoolArg("-bip39mnemonic", false);

    if (ui->rescanProgress) {
        ui->rescanProgress->setRange(0, 100);
        ui->rescanProgress->setValue(0);
        ui->rescanProgress->setVisible(false);
        ui->rescanProgress->setTextVisible(true);
    }
    if (ui->birthdayEdit) ui->birthdayEdit->setDate(QDate::currentDate());

    // Tooltips
    if (ui->sweepButton) {
        ui->sweepButton->setToolTip(tr("Sweep funds from this mnemonic into this wallet or "
                                       "a destination address, without changing the wallet seed."));
    }
    if (ui->okButton) {
        ui->okButton->setToolTip(tr("Import this mnemonic as the HD seed for this wallet. "));
    }

    wordModel  = new QStringListModel(this);
    completer  = new QCompleter(wordModel, this);
    completer->setCaseSensitivity(Qt::CaseInsensitive);

    ui->wordListComboBox->addItem("English",              "eng");
    ui->wordListComboBox->addItem("Chinese (simplified)", "sc");
    ui->wordListComboBox->addItem("Chinese (traditional)","tc");
    ui->wordListComboBox->addItem("Czech",                "cze");
    ui->wordListComboBox->addItem("French",               "fra");
    ui->wordListComboBox->addItem("Italian",              "ita");
    ui->wordListComboBox->addItem("Japanese",             "jpn");
    ui->wordListComboBox->addItem("Korean",               "kor");
    ui->wordListComboBox->addItem("Portuguese",           "por");
    ui->wordListComboBox->addItem("Spanish",              "spa");

    connect(ui->wordListComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &ImportBip39Dialog::onWordListChanged);

    setupLineEdits();
    loadWordList("eng");
    updateModeUi(); // apply sweep vs import visibility and text

    connect(ui->showMnemonicCheckBox, &QCheckBox::toggled,
            this, &ImportBip39Dialog::toggleShowMnemonic);
    connect(ui->showExtraCheckBox, &QCheckBox::toggled,
            this, &ImportBip39Dialog::toggleShowExtraWord);
    connect(ui->checkBox24, &QCheckBox::toggled,
            this, &ImportBip39Dialog::toggleExtraWords);
    connect(ui->walletPassEdit,        &QLineEdit::textChanged,
            this, &ImportBip39Dialog::setOkButtonState);
    connect(ui->walletPassConfirmEdit, &QLineEdit::textChanged,
            this, &ImportBip39Dialog::setOkButtonState);
    connect(ui->sweepButton,           &QPushButton::clicked,
            this, &ImportBip39Dialog::on_sweepButton_clicked);

    if (fPruneMode && ui->rescanCheckBox) {
        ui->rescanCheckBox->setEnabled(false);
        ui->rescanCheckBox->setToolTip(tr("Rescan is disabled in pruned mode."));
    }

    setOkButtonState();
}

ImportBip39Dialog::~ImportBip39Dialog()
{
    if (rescanThread.isRunning()) { rescanThread.quit(); rescanThread.wait(); }
    if (sweepThread_.isRunning()) { sweepThread_.quit(); sweepThread_.wait(); }
    detachProgress();
    delete ui;
}

void ImportBip39Dialog::loadWordList(const QString &langCode)
{
    currentWords.clear();
    const char* const* wl = nullptr;
    if (langCode == QLatin1String("eng"))      wl = wordlist_eng;
    else if (langCode == QLatin1String("jpn")) wl = wordlist_jpn;
    else if (langCode == QLatin1String("ita")) wl = wordlist_ita;
    else if (langCode == QLatin1String("fra")) wl = wordlist_fra;
    else if (langCode == QLatin1String("kor")) wl = wordlist_kor;
    else if (langCode == QLatin1String("por")) wl = wordlist_por;
    else if (langCode == QLatin1String("spa")) wl = wordlist_spa;
    else if (langCode == QLatin1String("cze") || langCode == QLatin1String("ces")) wl = wordlist_cze;
    else if (langCode == QLatin1String("sc")  || langCode == QLatin1String("chs")) wl = wordlist_sc;
    else if (langCode == QLatin1String("tc")  || langCode == QLatin1String("cht")) wl = wordlist_tc;

    if (wl) {
        currentWords.reserve(LANG_WORD_CNT);
        for (int i = 0; i < LANG_WORD_CNT; ++i)
            currentWords << QString::fromUtf8(wl[i]);
    }

    rebuildCompleter();
}

void ImportBip39Dialog::setupLineEdits()
{
    edits.reserve(24);
    for (int i = 0; i < 24; ++i) {
        QLineEdit* e = findChild<QLineEdit*>(QStringLiteral("word%1").arg(i));
        edits << e;
    }
    for (auto *e : edits) {
        if (!e) continue;
        e->setEchoMode(QLineEdit::Normal);
        e->setCompleter(completer);

        connect(e, &QLineEdit::editingFinished, this, [this, e]() {
            if (!e) return;
            if (ui->showMnemonicCheckBox->isChecked()) return;
            if (ui->disableAutohideCheckBox->isChecked()) return;
            QTimer::singleShot(5000, this, [this, e]() {
                if (!e) return;
                if (!ui->showMnemonicCheckBox->isChecked() &&
                    !ui->disableAutohideCheckBox->isChecked())
                    e->setEchoMode(QLineEdit::Password);
            });
        });
        connect(e, &QLineEdit::textChanged, this,
                &ImportBip39Dialog::setOkButtonState);
    }
    toggleExtraWords(false);
}

void ImportBip39Dialog::toggleExtraWords(bool show24)
{
    for (int i = 12; i < 24; ++i) {
        if (i < edits.size() && edits[i])
            edits[i]->setVisible(show24);
    }
    adjustSize();
    setOkButtonState();
}

void ImportBip39Dialog::toggleShowMnemonic(bool show)
{
    for (auto *e : edits) {
        if (e) e->setEchoMode(show ? QLineEdit::Normal : QLineEdit::Password);
    }
}

void ImportBip39Dialog::toggleShowExtraWord(bool show)
{
    if (ui->extraWordEdit)
        ui->extraWordEdit->setEchoMode(show ? QLineEdit::Normal : QLineEdit::Password);
}

void ImportBip39Dialog::onWordListChanged(int)
{
    loadWordList(ui->wordListComboBox->currentData().toString());
    setOkButtonState();
}

QString ImportBip39Dialog::collectMnemonic() const
{
    const int cnt = ui->checkBox24->isChecked() ? 24 : 12;
    QStringList words;
    words.reserve(cnt);

    for (int i = 0; i < cnt && i < edits.size(); ++i) {
        if (!edits[i]) continue;
        words << edits[i]->text().trimmed();
    }
    return words.join(' ');
}

QString ImportBip39Dialog::extraWord()        const
{
    return ui->extraWordEdit ? ui->extraWordEdit->text().trimmed() : QString();
}
QString ImportBip39Dialog::walletPassphrase() const
{
    return ui->walletPassEdit ? ui->walletPassEdit->text() : QString();
}
QString ImportBip39Dialog::keyPath()          const
{
    return ui->keyPathEdit ? ui->keyPathEdit->text().trimmed() : QString();
}

QString ImportBip39Dialog::sweepDestination() const
{
    if (ui && ui->sweepDestEdit)
        return ui->sweepDestEdit->text().trimmed();
    return QString();
}

void ImportBip39Dialog::resetDialogValues()
{
    for (auto *e : edits) {
        if (e) e->clear();
    }
    ui->checkBox24->setChecked(false);
    if (ui->extraWordEdit)        ui->extraWordEdit->clear();
    if (ui->walletPassEdit)       ui->walletPassEdit->clear();
    if (ui->walletPassConfirmEdit)ui->walletPassConfirmEdit->clear();
    if (ui->keyPathEdit)          ui->keyPathEdit->clear();
    if (ui->sweepDestEdit)        ui->sweepDestEdit->clear();
    ui->showMnemonicCheckBox->setChecked(false);
    ui->disableAutohideCheckBox->setChecked(false);
    ui->rescanCheckBox->setChecked(false);
    ui->statusLabel->clear();

    if (ui->rescanProgress) {
        ui->rescanProgress->setVisible(false);
        ui->rescanProgress->setValue(0);
        ui->rescanProgress->setRange(0, 100);
    }
    setOkButtonState();
}

void ImportBip39Dialog::setOkButtonState()
{
    const int need = ui->checkBox24->isChecked() ? 24 : 12;
    bool mnemonicLooksValid = true;

    for (int i = 0; i < need && mnemonicLooksValid; ++i) {
        if (i >= edits.size() || !edits[i]) {
            mnemonicLooksValid = false;
            break;
        }
        mnemonicLooksValid &=
            currentWords.contains(edits[i]->text().trimmed(), Qt::CaseInsensitive);
    }

    bool passMatch = ui->walletPassEdit &&
                     ui->walletPassConfirmEdit &&
                     !ui->walletPassEdit->text().isEmpty() &&
                     (ui->walletPassEdit->text() == ui->walletPassConfirmEdit->text());

    if (ui->okButton)
        ui->okButton->setEnabled(import_mode_ && mnemonicLooksValid && passMatch);

    if (ui->sweepButton)
        ui->sweepButton->setEnabled(!import_mode_ && mnemonicLooksValid);
}

void ImportBip39Dialog::on_resetButton_clicked()
{
    resetDialogValues();
}

void ImportBip39Dialog::on_cancelButton_clicked()
{
    reject();
}

CBlockIndex* ImportBip39Dialog::rescanStartIndex() const
{
    // "First activity date" is for sweep only. Import rescan always starts from genesis.
    if (ui->birthdayEdit) {
        QDate chosen = ui->birthdayEdit->date();
        if (chosen.isValid() && chosen < QDate::currentDate()) {
            // Qt5-safe epoch seconds
            int64_t ts = QDateTime(chosen, QTime(0,0), Qt::UTC).toTime_t();
            if (CBlockIndex* idx = chainActive.FindEarliestAtLeast(ts)) return idx;
        }
    }
    return chainActive.Genesis();
}

void ImportBip39Dialog::on_sweepButton_clicked()
{
#ifndef USE_BIP39
    ui->statusLabel->setText(tr("This binary was built without BIP-39 support."));
    return;
#else
    if (import_mode_) {
        return;
    }

    QString mnemonic = collectMnemonic();
    if (mnemonic.isEmpty()) {
        ui->statusLabel->setText(tr("Please enter a mnemonic."));
        return;
    }

    QString langCode = ui->wordListComboBox->currentData().toString();
    if (langCode.isEmpty()) {
        ui->statusLabel->setText(tr("Please select a word list."));
        return;
    }

    const std::string mnemonicStr  = mnemonic.toStdString();
    const std::string extraWordStr = extraWord().toStdString();
    const std::string basePath     =
        keyPath().toStdString().empty() ? "m/44'/3'/0'" : keyPath().toStdString();

    if (!pwalletMain->VerifyBip39Mnemonic(mnemonicStr.c_str(),
                                          langCode.toStdString().c_str(), " ")) {
        ui->statusLabel->setText(tr("Invalid mnemonic."));
        return;
    }

    // Optional destination address validation (sweep only)
    QString destQ = sweepDestination();
    if (!destQ.isEmpty()) {
        CBitcoinAddress d(destQ.toStdString());
        if (!d.IsValid()) {
            ui->statusLabel->setText(tr("Invalid destination address."));
            return;
        }
        cached_dest_ = destQ.toStdString();
    } else {
        cached_dest_.clear();
    }

    // Cache sweep parameters (first activity date is used here only)
    cached_mnemonic_ = mnemonicStr;
    cached_extra_    = extraWordStr;
    cached_basepath_ = basePath;
    cached_gap_      = BIP39_ADDR_GAP;
    cached_start_    = rescanStartIndex(); // uses first-activity-date for sweep

    startSweepWorker(/*dryRun=*/true);
#endif
}

void ImportBip39Dialog::startSweepWorker(bool dryRun)
{
    if (sweepThread_.isRunning()) return;

    setBusy(true, /*indeterminate=*/true,
            dryRun ? tr("Checking for funds at derived addresses (dry run)…")
                   : tr("Sweeping funds…"));
    attachProgress();

    auto *worker = new SweepWorker;
    worker->wallet   = pwalletMain;
    worker->mnemonic = cached_mnemonic_;
    worker->extra    = cached_extra_;
    worker->basePath = cached_basepath_;
    worker->gap      = cached_gap_;
    worker->dry_run  = dryRun;
    worker->dest_str = cached_dest_;   // pass optional destination string
    worker->startAt  = cached_start_;  // birthday start (sweep only)

    worker->moveToThread(&sweepThread_);
    connect(&sweepThread_, &QThread::started, worker, &SweepWorker::run);
    connect(worker, &SweepWorker::finished, this,
        [this, dryRun](bool ok, QString err, int inputs, qint64 total,
                       qint64 fee, QString sent_to, QString txid) {
            handleSweepFinished(ok, err, inputs, total, fee, sent_to, txid, dryRun);
        }, Qt::QueuedConnection);
    connect(&sweepThread_, &QThread::finished, worker, &QObject::deleteLater,
            Qt::DirectConnection);

    sweepThread_.start();
}

void ImportBip39Dialog::handleSweepFinished(bool ok, const QString& err,
                                            int inputs, qint64 total, qint64 fee,
                                            const QString& sent_to,
                                            const QString& txid, bool dryRun)
{
    detachProgress();
    setBusy(false, /*indeterminate=*/false, QString());

    if (!ok) {
        QMessageBox::warning(this,
                             tr("Sweep %1 failed").arg(dryRun ? tr("check")
                                                              : tr("transaction")),
                             err);
        ui->statusLabel->setText(dryRun ? tr("Dry-run sweep failed.")
                                        : tr("Sweep failed."));
        return;
    }

    if (dryRun) {
        if (inputs <= 0 || total <= 0) {
            QMessageBox::information(this, tr("No funds found"),
                                     tr("No UTXOs were found within the derived range."));
            ui->statusLabel->setText(tr("No funds to sweep."));
            return;
        }
        QString msg = tr("Found %1 input(s), total %2.\n\nDo you want to sweep these funds now?")
                      .arg(inputs)
                      .arg(QString::fromStdString(FormatMoney(total)));
        if (QMessageBox::question(this, tr("Sweep funds?"), msg,
                                  QMessageBox::Yes | QMessageBox::No,
                                  QMessageBox::Yes) != QMessageBox::Yes) {
            ui->statusLabel->setText(tr("Sweep cancelled."));
            return;
        }
        startSweepWorker(/*dryRun=*/false);
        return;
    }

    QString sentToDisplay = sent_to;
    if (sentToDisplay.isEmpty())
        sentToDisplay = tr("this wallet");

    QMessageBox::information(this, tr("Sweep complete"),
                             tr("Broadcast sweep transaction:\nTXID: %1\nSent to: %2\nFee: %3")
                             .arg(txid)
                             .arg(sentToDisplay)
                             .arg(QString::fromStdString(FormatMoney(fee))));
    ui->statusLabel->setText(tr("Sweep complete."));
}

void ImportBip39Dialog::on_okButton_clicked()
{
    if (importMnemonic()) {
        resetDialogValues();
        accept();
    }
}

bool ImportBip39Dialog::importMnemonic()
{
#ifndef USE_BIP39
    ui->statusLabel->setText(tr("This binary was built without BIP-39 support."));
    return false;
#else
    if (!import_mode_) {
        ui->statusLabel->setText(tr("Import is only available when started with -bip39mnemonic."));
        return false;
    }

    QString mnemonic   = collectMnemonic();
    QString extraWordS = this->extraWord();
    QString keyPathS   = this->keyPath();
    QString passphrase = this->walletPassphrase();

    if (mnemonic.isEmpty()) {
        ui->statusLabel->setText(tr("Please enter a mnemonic."));
        return false;
    }
    const int wordCount = mnemonic.split(' ', QString::SkipEmptyParts).size();
    if (wordCount != 12 && wordCount != 24) {
        ui->statusLabel->setText(tr("Please enter a valid 12- or 24-word mnemonic."));
        return false;
    }
    if (passphrase.isEmpty()) {
        ui->statusLabel->setText(tr("Please enter a wallet passphrase."));
        return false;
    }
    if (!ui->walletPassConfirmEdit ||
        passphrase != ui->walletPassConfirmEdit->text()) {
        ui->statusLabel->setText(tr("Passphrases do not match."));
        return false;
    }

    std::string mnemonicStr   = mnemonic.toStdString();
    std::string extraWordStr  = extraWordS.toStdString();
    std::string keyPathStr    = keyPathS.toStdString();
    std::string passphraseStr = passphrase.toStdString();

    if (pwalletMain->IsCrypted() && pwalletMain->IsHDEnabled()) {
        ui->statusLabel->setText(tr("The wallet is already HD and encrypted."));
        memory_cleanse(const_cast<char*>(mnemonicStr.data()),   mnemonicStr.size());
        memory_cleanse(const_cast<char*>(extraWordStr.data()),  extraWordStr.size());
        memory_cleanse(const_cast<char*>(passphraseStr.data()), passphraseStr.size());
        memory_cleanse(const_cast<char*>(keyPathStr.data()),    keyPathStr.size());
        return false;
    }

    QString langCode = ui->wordListComboBox->currentData().toString();
    if (langCode.isEmpty()) {
        ui->statusLabel->setText(tr("Please select a word list."));
        memory_cleanse(const_cast<char*>(mnemonicStr.data()),   mnemonicStr.size());
        memory_cleanse(const_cast<char*>(extraWordStr.data()),  extraWordStr.size());
        memory_cleanse(const_cast<char*>(passphraseStr.data()), passphraseStr.size());
        memory_cleanse(const_cast<char*>(keyPathStr.data()),    keyPathStr.size());
        return false;
    }
    if (!pwalletMain->VerifyBip39Mnemonic(mnemonicStr.c_str(),
                                          langCode.toStdString().c_str(), " ")) {
        ui->statusLabel->setText(tr("Invalid mnemonic."));
        memory_cleanse(const_cast<char*>(mnemonicStr.data()),   mnemonicStr.size());
        memory_cleanse(const_cast<char*>(extraWordStr.data()),  extraWordStr.size());
        memory_cleanse(const_cast<char*>(passphraseStr.data()), passphraseStr.size());
        memory_cleanse(const_cast<char*>(keyPathStr.data()),    keyPathStr.size());
        return false;
    }

    CPubKey masterPub = pwalletMain->GenerateBip39MasterKey(mnemonicStr.c_str(),
                                                            passphraseStr.c_str(),
                                                            extraWordStr.c_str(),
                                                            keyPathStr.c_str());

    memory_cleanse(const_cast<char*>(mnemonicStr.data()),   mnemonicStr.size());
    memory_cleanse(const_cast<char*>(extraWordStr.data()),  extraWordStr.size());
    memory_cleanse(const_cast<char*>(passphraseStr.data()), passphraseStr.size());
    memory_cleanse(const_cast<char*>(keyPathStr.data()),    keyPathStr.size());

    if (!masterPub.IsValid() || !pwalletMain->SetHDMasterKey(masterPub)) {
        ui->statusLabel->setText(tr("Failed to set HD master key."));
        return false;
    }

    // After setting the HD master key, pre-generate the "gap" number of
    // external receive addresses. This makes the imported wallet immediately
    // have the first gap addresses ready.
    pwalletMain->NewKeyPool();

    CPubKey first;
    bool first_set = false;

    for (int i = 0; i < BIP39_ADDR_GAP; ++i) {
        CPubKey k;
        if (!pwalletMain->GetKeyFromPool(k))
            break;

        if (!first_set) {
            first = k;
            first_set = true;
        }

        pwalletMain->SetAddressBook(k.GetID(), "", "receive");
    }

    if (first_set) {
        pwalletMain->SetDefaultKey(first);
    }

    if (ui->rescanCheckBox->isChecked()) {
        auto *exec = new ImportBip39Executor;
        exec->moveToThread(&rescanThread);
        connect(this, &ImportBip39Dialog::rescanWallet,
                exec, &ImportBip39Executor::rescan);
        connect(&rescanThread, &QThread::finished,
                exec, &QObject::deleteLater, Qt::DirectConnection);

        attachProgress();

        rescanThread.start();
        setBusy(true, /*indeterminate=*/false, tr("Rescanning…"));
        if (ui->rescanProgress) {
            ui->rescanProgress->setVisible(true);
            ui->rescanProgress->setValue(0);
            ui->rescanProgress->setRange(0, 100);
        }

        connect(&rescanThread, &QThread::finished, this, [this]() {
            if (ui->rescanProgress) {
                ui->rescanProgress->setValue(100);
                ui->rescanProgress->setVisible(false);
                ui->rescanProgress->setRange(0, 100);
            }
            detachProgress();
            setBusy(false, /*indeterminate=*/false, tr("Rescan complete."));
        }, Qt::QueuedConnection);

        Q_EMIT rescanWallet(pwalletMain, chainActive.Genesis());
    }

    QMessageBox::information(this, tr("Import Successful"),
                             tr("BIP-39 mnemonic imported successfully."));
    if (ui->walletPassEdit)        ui->walletPassEdit->clear();
    if (ui->walletPassConfirmEdit) ui->walletPassConfirmEdit->clear();
    if (ui->keyPathEdit)           ui->keyPathEdit->clear();
    if (ui->extraWordEdit)         ui->extraWordEdit->clear();
    for (auto *e : edits) {
        if (e) e->clear();
    }

    return true;
#endif
}

void ImportBip39Dialog::attachProgress()
{
    progress_conn_ = uiInterface.ShowProgress.connect(
        [this](const std::string& title, int nProgress) {
            QMetaObject::invokeMethod(this, "updateRescanProgress",
                                      Qt::QueuedConnection,
                                      Q_ARG(QString,
                                            QString::fromStdString(title)),
                                      Q_ARG(int, nProgress));
        });
}

void ImportBip39Dialog::detachProgress()
{
    progress_conn_.disconnect();
}

void ImportBip39Dialog::updateRescanProgress(const QString& title, int progress)
{
    if (!ui || !ui->rescanProgress) return;
    if (!title.isEmpty()) ui->statusLabel->setText(title);
    ui->rescanProgress->setVisible(true);
    if (progress < 0) {
        ui->rescanProgress->setRange(0, 0); // indeterminate
    } else {
        if (ui->rescanProgress->minimum() == 0 &&
            ui->rescanProgress->maximum() == 0)
            ui->rescanProgress->setRange(0, 100);
        if (progress < 0)  progress = 0;
        if (progress > 100) progress = 100;
        ui->rescanProgress->setValue(progress);
        if (progress >= 100) ui->rescanProgress->setVisible(false);
    }
}

void ImportBip39Dialog::setBusy(bool busy, bool indeterminate,
                                const QString& status)
{
    if (ui->okButton)     ui->okButton->setEnabled(!busy);
    if (ui->cancelButton) ui->cancelButton->setEnabled(!busy);
    if (ui->resetButton)  ui->resetButton->setEnabled(!busy);
    if (ui->sweepButton)  ui->sweepButton->setEnabled(!busy);

    // Local progress bar at the bottom
    if (ui->rescanProgress) {
        if (busy) {
            ui->rescanProgress->setVisible(true);
            if (indeterminate)
                ui->rescanProgress->setRange(0, 0); // spinning / marquee
        } else {
            // Reset to determinate idle state and hide
            if (ui->rescanProgress->minimum() == 0 &&
                ui->rescanProgress->maximum() == 0) {
                ui->rescanProgress->setRange(0, 100);
                ui->rescanProgress->setValue(0);
            }
            ui->rescanProgress->setVisible(false);
        }
    }

    if (!status.isEmpty())
        ui->statusLabel->setText(status);
}

void ImportBip39Dialog::rebuildCompleter()
{
    if (!wordModel || !completer) return;
    wordModel->setStringList(currentWords);
    completer->setModel(wordModel);
}

void ImportBip39Dialog::updateModeUi()
{
    if (!ui) return;

    if (import_mode_) {
        if (ui->step4Label)        ui->step4Label->hide();
        if (ui->lblDest)           ui->lblDest->hide();
        if (ui->sweepDestEdit)     ui->sweepDestEdit->hide();
        if (ui->lblBirthday)       ui->lblBirthday->hide();
        if (ui->birthdayEdit)      ui->birthdayEdit->hide();
        if (ui->birthdayHintLabel) ui->birthdayHintLabel->hide();
        if (ui->sweepButton)       ui->sweepButton->hide();

        if (ui->step5Label)
            ui->step5Label->setText(tr("Step 4: Import as HD wallet"));
        if (ui->modeHintLabel) {
            ui->modeHintLabel->setText(
                tr("This will set this mnemonic as the HD seed for this wallet. "));
        }

        if (ui->importGroupBox) ui->importGroupBox->setVisible(true);

        if (ui->okButton) {
            ui->okButton->setVisible(true);
            ui->okButton->setDefault(true);
        }
    } else {
        // Show sweep fields
        if (ui->step4Label)        ui->step4Label->show();
        if (ui->lblDest)           ui->lblDest->show();
        if (ui->sweepDestEdit)     ui->sweepDestEdit->show();
        if (ui->lblBirthday)       ui->lblBirthday->show();
        if (ui->birthdayEdit)      ui->birthdayEdit->show();
        if (ui->birthdayHintLabel) ui->birthdayHintLabel->show();
        if (ui->sweepButton) {
            ui->sweepButton->show();
            ui->sweepButton->setDefault(true);
        }

        if (ui->importGroupBox) ui->importGroupBox->hide();
        if (ui->step5Label)
            ui->step5Label->setText(tr("Step 5: Sweep funds"));
        if (ui->modeHintLabel) {
            ui->modeHintLabel->setText(
                tr("Click Sweep to search for funds from this mnemonic "
                   "and send them into this wallet or to the optional destination address."));
        }
        if (ui->okButton) {
            ui->okButton->setVisible(false);
            ui->okButton->setDefault(false);
        }
    }
}

#endif // USE_BIP39
