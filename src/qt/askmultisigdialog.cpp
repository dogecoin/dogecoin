#include "askmultisigdialog.h"
#include "newpubkeydialog.h"
#include "ui_askmultisigdialog.h"
#include "addresstablemodel.h"
#include <QPushButton>
#include <QMessageBox>

AskMultisigDialog::AskMultisigDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AskMultisigDialog)
{
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
    QString pubKey = model->getRawPubKey();
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
//     CReserveKey reservekey(pwalletMain);
//     CPubKey vchPubKey;
//     if (!reservekey.GetReservedKey(vchPubKey))
//         throw JSONRPCError(RPC_WALLET_KEYPOOL_RAN_OUT, "Error: Keypool ran out, please call keypoolrefill first");
// 
//     reservekey.KeepKey();
// 
//     CKeyID keyID = vchPubKey.GetID();
// 
//     return CBitcoinAddress(keyID).ToString();


//     LOCK2(cs_main, pwalletMain->cs_wallet);
// 
//     string strAccount;
//     if (params.size() > 2)
//         strAccount = AccountFromValue(params[2]);
// 
//     // Construct using pay-to-script-hash:
//     CScript inner = _createmultisig_redeemScript(params);
//     CScriptID innerID(inner);
//     pwalletMain->AddCScript(inner);
// 
//     pwalletMain->SetAddressBook(innerID, strAccount, "send");
//     return CBitcoinAddress(innerID).ToString();
    int nTotal = 3, nRequired = 2;
    QStringList addresses;
    if(idx == 1) {
        nTotal = ui->totalAdressesSpinBox->value();
        nRequired = ui->signaturesRequiredSpinBox->value();
        addresses = ui->foreignAdressesEdit->toPlainText().split(QRegExp("\\W+"), QString::SkipEmptyParts);
        if(addresses.count() > nTotal) {
            QMessageBox::critical("Error generating multisig:\nnumber of provided pubkeys exceeds number of requested total pubkeys");
            return QString();
        }
        for(QStringList::iterator it=addresses.begin(); it != addresses.end(); ++it) {
            // TODO: valiate *it pubkey
        }
        for(int i = addresses.count(); i < nTotal; ++i)
            addresses.append(model->getRawPubKey());
    }

    return QString("2NC5eUtoz2a1oQF2VeF6A2TVK1FEmkgCRRM");
}

QString AskMultisigDialog::getLabel()
{
    return QString("Multisig");
}
