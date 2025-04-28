// Copyright (c) 2021-2025 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#if defined(HAVE_CONFIG_H)
#include "config/bitcoin-config.h"
#endif

#include "importbip39dialog.h"
#include "ui_importbip39dialog.h"

#include "platformstyle.h"
#include "wallet/wallet.h"
#include "validation.h"
#include "chain.h"
#include "utiltime.h"

#include <QCompleter>
#include <QFile>
#include <QLineEdit>
#include <QThread>
#include <QTimer>
#include <QStringListModel>
#include <QMessageBox>
#include "util.h"
#include <QDateTime>
#include <QDir>

#ifdef USE_LIB
#include "support/experimental.h"

EXPERIMENTAL_FEATURE

class ImportBip39Executor : public QObject
{
    Q_OBJECT
public Q_SLOTS:
    void rescan(CWallet *wallet, CBlockIndex *genesis)
    {
        wallet->ScanForWalletTransactions(genesis, true);
        QThread::currentThread()->quit();
    }
};
#include "importbip39dialog.moc"

ImportBip39Dialog::ImportBip39Dialog(const PlatformStyle *platformStyle,
                                     const QString& cliMnemonic,
                                     const QString& cliPassphrase,
                                     const QString& cliKeyPath,
                                     const QString& cliExtraWord,
                                     QWidget *parent)
    : QDialog(parent),
      ui(new Ui::ImportBip39Dialog),
      style(platformStyle)
{
    ui->setupUi(this);

    wordModel  = new QStringListModel(this);
    completer  = new QCompleter(wordModel, this);
    completer->setCaseSensitivity(Qt::CaseInsensitive);

    ui->wordListComboBox->addItem("English",              "english.txt");
    ui->wordListComboBox->addItem("Chinese (simplified)", "chinese_simplified.txt");
    ui->wordListComboBox->addItem("Chinese (traditional)","chinese_traditional.txt");
    ui->wordListComboBox->addItem("Czech",                "czech.txt");
    ui->wordListComboBox->addItem("French",               "french.txt");
    ui->wordListComboBox->addItem("Italian",              "italian.txt");
    ui->wordListComboBox->addItem("Japanese",             "japanese.txt");
    ui->wordListComboBox->addItem("Korean",               "korean.txt");
    ui->wordListComboBox->addItem("Portuguese",           "portuguese.txt");
    ui->wordListComboBox->addItem("Spanish",              "spanish.txt");

    connect(ui->wordListComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &ImportBip39Dialog::onWordListChanged);

    setupLineEdits();
    loadWordList("english.txt");

    connect(ui->showMnemonicCheckBox, &QCheckBox::toggled, this, &ImportBip39Dialog::toggleShowMnemonic);
    connect(ui->showExtraCheckBox, &QCheckBox::toggled, this, &ImportBip39Dialog::toggleShowExtraWord);
    connect(ui->checkBox24, &QCheckBox::toggled, this, &ImportBip39Dialog::toggleExtraWords);

    connect(ui->walletPassEdit,        &QLineEdit::textChanged, this, &ImportBip39Dialog::setOkButtonState);
    connect(ui->walletPassConfirmEdit, &QLineEdit::textChanged, this, &ImportBip39Dialog::setOkButtonState);

    if (fPruneMode) ui->rescanCheckBox->setEnabled(false);

    if (!cliMnemonic.isEmpty()) {
        QStringList words = cliMnemonic.split(QRegExp("\\s+"),
                                              QString::SkipEmptyParts);
        const bool use24 = (words.size() == 24);
        ui->checkBox24->setChecked(use24);
        toggleExtraWords(use24);
        for (int i = 0; i < words.size() && i < 24; ++i)
            edits[i]->setText(words[i]);
        ui->showMnemonicCheckBox->setChecked(true);
    }
    if (!cliPassphrase.isEmpty()) ui->walletPassEdit->setText(cliPassphrase);
    if (!cliExtraWord.isEmpty()) ui->extraWordEdit->setText(cliExtraWord);
    if (!cliKeyPath.isEmpty()) ui->keyPathEdit->setText(cliKeyPath);

    setOkButtonState();
}

ImportBip39Dialog::~ImportBip39Dialog()
{
    if (rescanThread.isRunning()) {
        rescanThread.quit();
        rescanThread.wait();
    }
    delete ui;
}

void ImportBip39Dialog::loadWordList(const QString &fileAlias)
{
    currentWords.clear();
    QFile f(":/wordlists/" + fileAlias);
    if (f.open(QIODevice::ReadOnly | QIODevice::Text))
        while (!f.atEnd())
            currentWords << QString::fromUtf8(f.readLine()).trimmed();
    wordModel->setStringList(currentWords);
    completer->setModel(wordModel);
}

void ImportBip39Dialog::setupLineEdits()
{
    edits.reserve(24);
    for (int i = 0; i < 24; ++i) {
        auto *le = findChild<QLineEdit*>(QStringLiteral("word%1").arg(i));
        edits << le;
    }

    for (auto *e : edits) {
        e->setEchoMode(QLineEdit::Normal);
        e->setCompleter(completer);

        connect(e, &QLineEdit::editingFinished, this, [this, e]() {
            if (ui->showMnemonicCheckBox->isChecked()) return;
            if (ui->disableAutohideCheckBox->isChecked()) return;
            QTimer::singleShot(5000, this, [this, e]() {
                if (!ui->showMnemonicCheckBox->isChecked() &&
                    !ui->disableAutohideCheckBox->isChecked())
                    e->setEchoMode(QLineEdit::Password);
            });
        });

        connect(e, &QLineEdit::textChanged, this, &ImportBip39Dialog::setOkButtonState);
    }

    toggleExtraWords(false);
}

void ImportBip39Dialog::toggleExtraWords(bool show24)
{
    for (int i = 12; i < 24; ++i) edits[i]->setVisible(show24);
    adjustSize();
    setOkButtonState();
}

void ImportBip39Dialog::toggleShowMnemonic(bool show)
{
    for (auto *e : edits) e->setEchoMode(show ? QLineEdit::Normal : QLineEdit::Password);
}

void ImportBip39Dialog::toggleShowExtraWord(bool show)
{
    ui->extraWordEdit->setEchoMode(show ? QLineEdit::Normal : QLineEdit::Password);
}

void ImportBip39Dialog::onWordListChanged(int index)
{
    loadWordList(ui->wordListComboBox->itemData(index).toString());
    setOkButtonState();
}

QString ImportBip39Dialog::collectMnemonic() const
{
    const int cnt = ui->checkBox24->isChecked() ? 24 : 12;
    QStringList words;
    for (int i = 0; i < cnt; ++i) words << edits[i]->text().trimmed();
    return words.join(' ');
}

QString ImportBip39Dialog::extraWord()        const { return ui->extraWordEdit->text().trimmed(); }
QString ImportBip39Dialog::walletPassphrase() const { return ui->walletPassEdit->text();          }
QString ImportBip39Dialog::keyPath()          const { return ui->keyPathEdit->text().trimmed();   }

void ImportBip39Dialog::resetDialogValues()
{
    for (auto *e : edits) e->clear();
    ui->checkBox24->setChecked(false);
    ui->extraWordEdit->clear();
    ui->walletPassEdit->clear();
    ui->walletPassConfirmEdit->clear();
    ui->keyPathEdit->clear();
    ui->showMnemonicCheckBox->setChecked(false);
    ui->disableAutohideCheckBox->setChecked(false);
    ui->rescanCheckBox->setChecked(false);
    ui->statusLabel->clear();
    setOkButtonState();
}

void ImportBip39Dialog::setOkButtonState()
{
    const int need = ui->checkBox24->isChecked() ? 24 : 12;
    bool ok = true;
    for (int i = 0; i < need && ok; ++i)
        ok &= currentWords.contains(edits[i]->text().trimmed(), Qt::CaseInsensitive);
    bool passMatch = !ui->walletPassEdit->text().isEmpty() &&
                     (ui->walletPassEdit->text() ==
                      ui->walletPassConfirmEdit->text());
    ui->okButton->setEnabled(ok && passMatch);
}

void ImportBip39Dialog::on_resetButton_clicked()  { resetDialogValues(); }
void ImportBip39Dialog::on_cancelButton_clicked() { reject(); }

void ImportBip39Dialog::on_okButton_clicked()
{
    if (importMnemonic()) {
        resetDialogValues();
        accept();
    }
}

bool ImportBip39Dialog::importMnemonic()
{
#ifndef USE_LIB
    ui->statusLabel->setText(tr("This binary was built without libdogecoin / BIP-39 support."));
    return false;
#else

    // Copy the strings into locals
    QString mnemonic = collectMnemonic();
    QString extraWord = this->extraWord();
    QString keyPath = this->keyPath();
    QString passphrase = walletPassphrase();
    if (mnemonic.isEmpty()) {
        ui->statusLabel->setText(tr("Please enter a mnemonic."));
        return false;
    }
    if (mnemonic.split(' ').size() != 12 && mnemonic.split(' ').size() != 24) {
        ui->statusLabel->setText(tr("Please enter a valid mnemonic."));
        return false;
    }
    if (passphrase.isEmpty()) {
        ui->statusLabel->setText(tr("Please enter a wallet passphrase."));
        return false;
    }
    if (passphrase != ui->walletPassConfirmEdit->text()) {
        ui->statusLabel->setText(tr("Passphrases do not match."));
        return false;
    }

    std::string mnemonicStr = mnemonic.toStdString();
    std::string extraWordStr = extraWord.toStdString();
    std::string keyPathStr = keyPath.toStdString();
    std::string passphraseStr = passphrase.toStdString();

    // Check if the wallet is already HD/encrypted
    if (pwalletMain->IsCrypted() && pwalletMain->IsHDEnabled()) {
        ui->statusLabel->setText(tr("The wallet is already HD and encrypted."));
        return false;
    }

    // Check if mnemonic is valid
    if (!pwalletMain->VerifyBip39Mnemonic(mnemonicStr.c_str(),
                                          "eng",
                                          " ",
                                          nullptr)) {
        ui->statusLabel->setText(tr("Invalid mnemonic."));
        return false;
    }

    CPubKey masterPub = pwalletMain->GenerateBip39MasterKey(mnemonicStr.c_str(),
                                                            passphraseStr.c_str(),
                                                            extraWordStr.c_str(),
                                                            keyPathStr.c_str());

    memory_cleanse(const_cast<char*>(mnemonicStr.data()), mnemonicStr.size());
    memory_cleanse(const_cast<char*>(extraWordStr.data()), extraWordStr.size());
    memory_cleanse(const_cast<char*>(passphraseStr.data()), passphraseStr.size());
    memory_cleanse(const_cast<char*>(keyPathStr.data()),   keyPathStr.size());

    if (!masterPub.IsValid() || !pwalletMain->SetHDMasterKey(masterPub))
    {
        ui->statusLabel->setText(tr("Failed to set HD master key."));
        return false;
    }

    pwalletMain->NewKeyPool();
    CPubKey def;
    if (pwalletMain->GetKeyFromPool(def)) {
        pwalletMain->SetDefaultKey(def);
        pwalletMain->SetAddressBook(def.GetID(), "", "receive");
    }

    if (ui->rescanCheckBox->isChecked()) {
        auto *exec = new ImportBip39Executor;
        exec->moveToThread(&rescanThread);
        connect(this, &ImportBip39Dialog::rescanWallet, exec, &ImportBip39Executor::rescan);
        connect(&rescanThread, &QThread::finished, exec, &QObject::deleteLater, Qt::DirectConnection);
        rescanThread.start();
        ui->statusLabel->setText(tr("Rescanning…"));
        Q_EMIT rescanWallet(pwalletMain, chainActive.Genesis());
    }

    QMessageBox::information(this, tr("Import Successful"), tr("BIP-39 mnemonic imported successfully."));

    // Clear the fields
    ui->walletPassEdit->clear();
    ui->walletPassConfirmEdit->clear();
    ui->keyPathEdit->clear();
    ui->extraWordEdit->clear();
    for (auto *e : edits) e->clear();

    return true;
#endif
}

#endif // USE_LIB
