#include "askmultisigdialog.h"
#include "newpubkeydialog.h"
#include "ui_askmultisigdialog.h"
#include "addresstablemodel.h"
#include "utilstrencodings.h"
#include "pubkey.h"
#include "script/standard.h"
#include "util.h"

#include <QPushButton>
#include <QMessageBox>

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

QString AskMultisigDialog::generateAddress()
{
    int idx = ui->toolBox->currentIndex();
    if(idx > 1)
        return QString();

    int nTotal = 3, nRequired = 2;
    QStringList addresses;
    try {
        if(idx == 1) {
            nTotal = ui->totalAdressesSpinBox->value();
            nRequired = ui->signaturesRequiredSpinBox->value();
            addresses = ui->foreignAdressesEdit->toPlainText().split(QRegExp("\\W+"), QString::SkipEmptyParts);
            if (addresses.count() > nTotal)
                throw std::runtime_error("Number of provided pubkeys exceeds number of requested total pubkeys.");
        } else {
            QString counterpartyAddress = ui->counterpartyAddressLineEdit->text().trimmed();
            if(counterpartyAddress.isEmpty())
                throw std::runtime_error("Counterparty pubkey is required.");
            addresses << counterpartyAddress;
            QString escrowAddress = ui->escrowAddressLineEdit->text().trimmed();
            if(!escrowAddress.isEmpty())
                addresses << escrowAddress;
            nTotal = addresses.size() + 1;
        }

        std::vector<CPubKey> pubkeys;
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

        CScript script = GetScriptForMultisig(nRequired, pubkeys);
        if (script.size() > MAX_SCRIPT_ELEMENT_SIZE)
            throw std::runtime_error(
                    strprintf("redeemScript exceeds size limit: %d > %d", script.size(), MAX_SCRIPT_ELEMENT_SIZE)
            );
        CScriptID scriptID(script);
        CBitcoinAddress address(scriptID);

        Object result;
        result.push_back(Pair("address", address.ToString()));
        result.push_back(Pair("redeemScript", HexStr(inner.begin(), inner.end())));

//     CScript inner = _createmultisig_redeemScript(params);
//     CScriptID innerID(inner);
//     pwalletMain->AddCScript(inner);
//
//     pwalletMain->SetAddressBook(innerID, strAccount, "send");
//     return CBitcoinAddress(innerID).ToString();
    } catch (std::runtime_error err) {
        QMessageBox::critical(this, QString("Error generating multisig"), QString(err.what()));
    }

    return QString("2NC5eUtoz2a1oQF2VeF6A2TVK1FEmkgCRRM");
}

QString AskMultisigDialog::getLabel()
{
    return QString("Multisig");
}
