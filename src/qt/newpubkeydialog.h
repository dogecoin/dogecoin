#ifndef NEWPUBKEYDIALOG_H
#define NEWPUBKEYDIALOG_H

#include <QDialog>

namespace Ui {
class NewPubKeyDialog;
}

class NewPubKeyDialog : public QDialog
{
    Q_OBJECT

public:
    explicit NewPubKeyDialog(QWidget *parent, QString &pubKey);
    ~NewPubKeyDialog();

private:
    Ui::NewPubKeyDialog *ui;
};

#endif // NEWPUBKEYDIALOG_H
