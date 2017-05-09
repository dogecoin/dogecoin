#ifndef ASKMULTISIGDIALOG_H
#define ASKMULTISIGDIALOG_H

#include <QDialog>
#include <list>

class AddressTableModel;

namespace Ui {
class AskMultisigDialog;
}

class AskMultisigDialog : public QDialog
{
    Q_OBJECT

public:
    explicit AskMultisigDialog(QWidget *parent = 0);
    ~AskMultisigDialog();
    void setModel(AddressTableModel *model) { this->model = model; }
    QString generateAddress(QString);

    // These both should be called only after calling generateAddress
    QString getLabel() { return _label; }
    QString getRedeemScript() { return _redeemScript; }

private:
    Ui::AskMultisigDialog *ui;
    AddressTableModel *model;
    QString _label;
    QString _redeemScript;

private slots:
    void generatePubKey();
};

#endif // ASKMULTISIGDIALOG_H
