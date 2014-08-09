// Copyright (c) 2011-2014 The Bitcoin developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "utilitydialog.h"

#include "ui_aboutdialog.h"
#include "ui_paperwalletdialog.h"
#include "ui_helpmessagedialog.h"

#include "bitcoinunits.h"
#include "sendcoinsdialog.h"
#include "sendcoinsentry.h"
#include "coincontrol.h"
#include "coincontroldialog.h"

#include "optionsmodel.h"
#include "bitcoingui.h"
#include "clientmodel.h"
#include "guiutil.h"

#include "clientversion.h"
#include "init.h"
#include "util.h"

#include <QLabel>
#include <QVBoxLayout>
#include <QInputDialog>

#ifdef USE_QRCODE
#include <qrencode.h>
#endif

#include <QtPrintSupport/QPrinter>
#include <QPainter>
#include <QPrintPreviewDialog>
#include <QPrintDialog>
#include <QGraphicsScene>
#include "walletmodel.h"


/** "About" dialog box */
AboutDialog::AboutDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AboutDialog)
{
    ui->setupUi(this);

    // Set current copyright year
    ui->copyrightLabel->setText(tr("Copyright") + QString(" &copy; 2009-%1 ").arg(COPYRIGHT_YEAR) + tr("The Bitcoin Core developers") + QString("<br>") + tr("Copyright") + QString(" &copy; 2013-%1 ").arg(COPYRIGHT_YEAR) + tr("The Dogecoin developers"));
}

void AboutDialog::setModel(ClientModel *model)
{
    if(model)
    {
        QString version = model->formatFullVersion();
        /* On x86 add a bit specifier to the version so that users can distinguish between
         * 32 and 64 bit builds. On other architectures, 32/64 bit may be more ambigious.
         */
#if defined(__x86_64__)
        version += " " + tr("(%1-bit)").arg(64);
#elif defined(__i386__ )
        version += " " + tr("(%1-bit)").arg(32);
#endif
        ui->versionLabel->setText(version);
    }
}

AboutDialog::~AboutDialog()
{
    delete ui;
}

void AboutDialog::on_buttonBox_accepted()
{
    close();
}

/** "PaperWallet" dialog box */
PaperWalletDialog::PaperWalletDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::PaperWalletDialog)
{
    ui->setupUi(this);

    ui->buttonBox->addButton(tr("Close"), QDialogButtonBox::RejectRole);

}

void PaperWalletDialog::setModel(WalletModel *model)
{
    RandAddSeed();
    this->model = model;
    this->on_getNewAddress_clicked();
}

PaperWalletDialog::~PaperWalletDialog()
{
    delete ui;
}

void PaperWalletDialog::on_getNewAddress_clicked()
{
    // Create a new private key
    CKey privKey;
    privKey.MakeNewKey(false);

    // Derive the public key and compress it
    CPubKey pubkey = privKey.GetPubKey();
    pubkey.Compress();

    // Derive the public key hash
    CBitcoinAddress pubkeyhash;
    pubkeyhash.Set(pubkey.GetID());

    // Create String versions of each
    string myPrivKey = CBitcoinSecret(privKey).ToString();
    string myPubKey = HexStr(pubkey.begin(), pubkey.end());
    string myAddress = pubkeyhash.ToString();


    // Generate the address QR code
    QRcode *code = QRcode_encodeString(myAddress.c_str(), 0, QR_ECLEVEL_M, QR_MODE_8, 1);
    if (!code)
    {
        ui->addressQRCode->setText(tr("Error encoding Address into QR Code."));
        return;
    }
    QImage myImage = QImage(code->width, code->width, QImage::Format_ARGB32);
    myImage.fill(QColor(0,0,0,0));
    unsigned char *p = code->data;
    for (int y = 0; y < code->width; y++)
    {
        for (int x = 0; x < code->width; x++)
        {
            myImage.setPixel(x, y, ((*p & 1) ? 0xff000000 : 0x0));
            p++;
        }
    }
    QRcode_free(code);


    // Generate the private key QR code
    code = QRcode_encodeString(myPrivKey.c_str(), 0, QR_ECLEVEL_M, QR_MODE_8, 1);
    if (!code)
    {
        ui->privateKeyQRCode->setText(tr("Error encoding private key into QR Code."));
        return;
    }
    QImage myImagePriv = QImage(code->width, code->width, QImage::Format_ARGB32);
    myImagePriv.fill(QColor(0,0,0,0));
    p = code->data;
    for (int y = 0; y < code->width; y++)
    {
        for (int x = 0; x < code->width; x++)
        {
            myImagePriv.setPixel(x, y, ((*p & 1) ? 0xff000000 : 0x0));
            p++;
        }
    }
    QRcode_free(code);

    // Populate the QR Codes and text
    ui->addressQRCode->setPixmap(QPixmap::fromImage(myImage).scaled(ui->addressQRCode->width(), ui->addressQRCode->height()));
    ui->addressText->setText(tr(myAddress.c_str()));

    ui->privateKeyQRCode->setPixmap(QPixmap::fromImage(myImagePriv).scaled(ui->privateKeyQRCode->width(), ui->privateKeyQRCode->height()));
    ui->privateKeyText->setText(tr(myPrivKey.c_str()));

    ui->publicKey->setHtml(myPubKey.c_str());

}

void PaperWalletDialog::on_printButton_clicked()
{

    QPrinter printer(QPrinter::HighResolution);
    QPrintDialog *qpd = new QPrintDialog(&printer, this);

    qpd->setEnabledOptions(QAbstractPrintDialog::PrintToFile);
    qpd->setPrintRange(QAbstractPrintDialog::AllPages);

    QList<QString> recipientPubKeyHashes;

    if ( qpd->exec() == QDialog::Accepted ) {

        QPainter painter;
        if (! painter.begin(&printer)) { // failed to open file
            qWarning("failed to open file, is it writable?");
            return;
        }

	int walletCount = ui->walletCount->currentIndex() + 1;

        int pageHeight = printer.pageRect().height();
        int walletHeight = ui->paperTemplate->height();
        double computedWalletHeight = 0.9 * pageHeight / 3;
        double scale = computedWalletHeight / walletHeight;
        double walletPadding = pageHeight * 0.05 / scale;

        QRegion walletRegion = QRegion(ui->paperTemplate->x(), ui->paperTemplate->y(),
        ui->paperTemplate->width(), ui->paperTemplate->height());
        painter.scale(scale, scale);

	for(int i = 0; i < walletCount; i++) {

            cout << "Starting to generate wallet #" << i << "\n";
            this->on_getNewAddress_clicked();
            QPoint point = QPoint(0, ( i % 3 ) * (walletHeight + walletPadding));
            this->render(&painter, point, walletRegion);
	    recipientPubKeyHashes.append(ui->addressText->text());

            cout << "Generated wallet #" << i << "\n";

            if ( i % 3 == 2 ) {

                printer.newPage();
                cout << "Next Page\n";

            }

	}

        painter.end();

    }

    bool ok;

    QString amountInput = QInputDialog::getText(this, "Load Wallets", "Please wait for wallets to print and verify readability.<br/>Enter the number of DOGE you wish to send to each wallet:", QLineEdit::Normal, QString(), &ok);

    if(!ok) {
        return;
    }

    quint64 amount = amountInput.toULongLong() * COIN;

    WalletModel::UnlockContext ctx(this->model->requestUnlock());
    if(!ctx.isValid())
    {
        return;
    }

    QList<SendCoinsRecipient> recipients;
    QStringList formatted;
    foreach(const QString &dest, recipientPubKeyHashes)
    {

        recipients.append(SendCoinsRecipient(dest,tr("Paper wallet %1").arg(dest), amount,""));
        formatted.append(tr("<b>%1</b> to Paper Wallet <span style='font-family: monospace;'>%2</span>").arg(amountInput,GUIUtil::HtmlEscape(dest)));

    }

    WalletModelTransaction tx(recipients);

    WalletModel::SendCoinsReturn prepareStatus;
    if (this->model->getOptionsModel()->getCoinControlFeatures()) // coin control enabled
        prepareStatus = this->model->prepareTransaction(tx, CoinControlDialog::coinControl);
    else
        prepareStatus = this->model->prepareTransaction(tx);

    if(prepareStatus.status != WalletModel::OK) {
	cout << "Wallet model ! == OK\n";
        return;
    }

   // Stolen from sendcoinsdialog.cpp
    qint64 txFee = tx.getTransactionFee();
    QString questionString = tr("Are you sure you want to send?");
    questionString.append("<br /><br />%1");

    if(txFee > 0)
    {
        // append fee string if a fee is required
        questionString.append("<hr /><span style='color:#aa0000;'>");
        questionString.append(BitcoinUnits::formatWithUnit(model->getOptionsModel()->getDisplayUnit(), txFee));
        questionString.append("</span> ");
        questionString.append(tr("added as transaction fee"));
    }

    // add total amount in all subdivision units
    questionString.append("<hr />");
    qint64 totalAmount = tx.getTotalTransactionAmount() + txFee;
    QStringList alternativeUnits;
    foreach(BitcoinUnits::Unit u, BitcoinUnits::availableUnits())
    {
        if(u != model->getOptionsModel()->getDisplayUnit())
            alternativeUnits.append(BitcoinUnits::formatWithUnit(u, totalAmount));
    }

    questionString.append(tr("Total Amount %1 (= %2)")
        .arg(BitcoinUnits::formatWithUnit(model->getOptionsModel()->getDisplayUnit(), totalAmount))
        .arg(alternativeUnits.join(" " + tr("or") + " ")));

    QMessageBox::StandardButton retval = QMessageBox::question(this, tr("Confirm send coins"),
        questionString.arg(formatted.join("<br />")),
        QMessageBox::Yes | QMessageBox::Cancel,
        QMessageBox::Cancel);

    if(retval != QMessageBox::Yes)
    {
        return;
    }

    WalletModel::SendCoinsReturn sendStatus = this->model->sendCoins(tx);

    return;

}

/** "Help message" dialog box */
HelpMessageDialog::HelpMessageDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::HelpMessageDialog)
{
    ui->setupUi(this);
    GUIUtil::restoreWindowGeometry("nHelpMessageDialogWindow", this->size(), this);

    header = tr("Dogecoin Core") + " " + tr("version") + " " +
        QString::fromStdString(FormatFullVersion()) + "\n\n" +
        tr("Usage:") + "\n" +
        "  dogecoin-qt [" + tr("command-line options") + "]                     " + "\n";

    coreOptions = QString::fromStdString(HelpMessage(HMM_BITCOIN_QT));

    uiOptions = tr("UI options") + ":\n" +
        "  -choosedatadir            " + tr("Choose data directory on startup (default: 0)") + "\n" +
        "  -lang=<lang>              " + tr("Set language, for example \"de_DE\" (default: system locale)") + "\n" +
        "  -min                      " + tr("Start minimized") + "\n" +
        "  -rootcertificates=<file>  " + tr("Set SSL root certificates for payment request (default: -system-)") + "\n" +
        "  -splash                   " + tr("Show splash screen on startup (default: 1)");

    ui->helpMessageLabel->setFont(GUIUtil::bitcoinAddressFont());

    // Set help message text
    ui->helpMessageLabel->setText(header + "\n" + coreOptions + "\n" + uiOptions);
}

HelpMessageDialog::~HelpMessageDialog()
{
    GUIUtil::saveWindowGeometry("nHelpMessageDialogWindow", this);
    delete ui;
}

void HelpMessageDialog::printToConsole()
{
    // On other operating systems, the expected action is to print the message to the console.
    QString strUsage = header + "\n" + coreOptions + "\n" + uiOptions + "\n";
    fprintf(stdout, "%s", strUsage.toStdString().c_str());
}

void HelpMessageDialog::showOrPrint()
{
#if defined(WIN32)
        // On Windows, show a message box, as there is no stderr/stdout in windowed applications
        exec();
#else
        // On other operating systems, print help text to console
        printToConsole();
#endif
}

void HelpMessageDialog::on_okButton_accepted()
{
    close();
}


/** "Shutdown" window */
void ShutdownWindow::showShutdownWindow(BitcoinGUI *window)
{
    if (!window)
        return;

    // Show a simple window indicating shutdown status
    QWidget *shutdownWindow = new QWidget();
    QVBoxLayout *layout = new QVBoxLayout();
    layout->addWidget(new QLabel(
        tr("Dogecoin Core is shutting down...") + "<br /><br />" +
        tr("Do not shut down the computer until this window disappears.")));
    shutdownWindow->setLayout(layout);

    // Center shutdown window at where main window was
    const QPoint global = window->mapToGlobal(window->rect().center());
    shutdownWindow->move(global.x() - shutdownWindow->width() / 2, global.y() - shutdownWindow->height() / 2);
    shutdownWindow->show();
}
