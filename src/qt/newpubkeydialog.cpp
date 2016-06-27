#include "newpubkeydialog.h"
#include "ui_newpubkeydialog.h"

NewPubKeyDialog::NewPubKeyDialog(QWidget *parent, QString &pubKey) :
    QDialog(parent),
    ui(new Ui::NewPubKeyDialog)
{
    ui->setupUi(this);
    ui->lineEdit->setText(pubKey);
}

NewPubKeyDialog::~NewPubKeyDialog()
{
    delete ui;
}