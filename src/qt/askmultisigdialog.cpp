#include "askmultisigdialog.h"
#include "newpubkeydialog.h"
#include "ui_askmultisigdialog.h"
#include "addresstablemodel.h"
#include "utilstrencodings.h"
#include "base58.h"
#include "pubkey.h"
#include "script/standard.h"
#include "util.h"

#include <QPushButton>
#include <QMessageBox>
#include <QTextStream>

#include <stdexcept>
#include <assert.h>


AskMultisigDialog::AskMultisigDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AskMultisigDialog)
{
    model = NULL;
    ui->setupUi(this);
    QObject::connect(ui->buttonBox->addButton(
        QString("&Generate new pubkey"),
        QDialogButtonBox::ActionRole
    ), SIGNAL(clicked()), this, SLOT(generatePubKey()));
    ui->buttonBox->button(QDialogButtonBox::Ok)->setDefault(true);
}

AskMultisigDialog::~AskMultisigDialog()
{
    delete ui;
}

void AskMultisigDialog::generatePubKey()
{
    assert(model != NULL);
    QString pubKey = model->getRawPubKeyString();
    if(pubKey.isNull())
    {
        QMessageBox::critical(this, QString("Something went wrong"),
                                    QString("Error while generating PubKey."));
    } else {
        NewPubKeyDialog dlg(this, pubKey);
        dlg.exec();
    }
}

QString AskMultisigDialog::generateAddress(QString label)
{
    int idx = ui->toolBox->currentIndex();

    int nTotal = 3, nRequired = 2;
    QStringList addresses;
    try {
        CScript script;
        if (idx > 1) {
            QString scriptText = ui->importRedeemScriptEdit->toPlainText().trimmed();
            std::vector<unsigned char> vScript = ParseHex(scriptText.toLatin1().data());
            script = CScript(vScript.begin(), vScript.end());

            _label = label.isEmpty() ? QString("Imported multisig") : label;
        } else {
            if (idx == 1) {
                nTotal = ui->totalAdressesSpinBox->value();
                nRequired = ui->signaturesRequiredSpinBox->value();
                if (nRequired > nTotal)
                    throw std::runtime_error("Number of requires signatures must not exceed number of total pubkeys.");
                addresses = ui->foreignAdressesEdit->toPlainText().split(QRegExp("\\W+"), QString::SkipEmptyParts);
                if (addresses.count() > nTotal)
                    throw std::runtime_error("Number of provided pubkeys exceeds number of requested total pubkeys.");
            } else {
                QString counterpartyAddress = ui->counterpartyAddressLineEdit->text().trimmed();
                if (counterpartyAddress.isEmpty())
                    throw std::runtime_error("Counterparty pubkey is required.");
                addresses << counterpartyAddress;
                QString escrowAddress = ui->escrowAddressLineEdit->text().trimmed();
                if (!escrowAddress.isEmpty())
                    addresses << escrowAddress;
                nTotal = addresses.size() + 1;
            }

            std::vector <CPubKey> pubkeys;
            pubkeys.resize(nTotal);
            int i = 0;

            // Validate addresses
            for (QStringList::iterator it = addresses.begin(); it != addresses.end(); ++it) {
                std::string ks = it->toStdString();
                if (!IsHex(ks))
                    throw std::runtime_error("Invalid public key: " + ks);

                CPubKey vchPubKey(ParseHex(ks));
                if (!vchPubKey.IsFullyValid())
                    throw std::runtime_error("Invalid public key: " + ks);

                pubkeys[i++] = vchPubKey;
            }

            // Generate any missing addresses
            for (; i < nTotal; ++i)
                pubkeys[i] = model->getRawPubKey();

            // Save label
            if (label.isEmpty()) {
                _label = QString();
                QTextStream(&_label) << nRequired << '/' << nTotal << " multisig";
            } else {
                _label = label;
            }

            // Generate multisig script
            script = GetScriptForMultisig(nRequired, pubkeys);
        }

        // Generate address from script
        if (script.size() > MAX_SCRIPT_ELEMENT_SIZE)
            throw std::runtime_error(
                    strprintf("redeemScript exceeds size limit: %d > %d", script.size(), MAX_SCRIPT_ELEMENT_SIZE)
            );
        CScriptID scriptID(script);
        CBitcoinAddress address(scriptID);
        _redeemScript = QString::fromStdString(HexStr(script.begin(), script.end()));
        if(_redeemScript.isEmpty())
            throw std::runtime_error("Invalid redeem script.");

        // Save
        model->saveReceiveScript(script, scriptID, _label);
        return QString::fromStdString(address.ToString());
    } catch (std::runtime_error err) {
        QMessageBox::critical(this, QString("Error generating multisig"), QString(err.what()));

        return QString();
    }
}
