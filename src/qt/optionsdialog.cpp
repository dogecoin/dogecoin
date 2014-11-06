// Copyright (c) 2011-2013 The Bitcoin developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#if defined(HAVE_CONFIG_H)
#include "bitcoin-config.h"
#endif

#include "optionsdialog.h"
#include "ui_optionsdialog.h"

#include "bitcoinunits.h"
#include "guiutil.h"
#include "monitoreddatamapper.h"
#include "optionsmodel.h"

#include "main.h" // for CTransaction::nMinTxFee and MAX_SCRIPTCHECK_THREADS
#include "netbase.h"
#include "txdb.h" // for -dbcache defaults

#include <QDir>
#include <QIntValidator>
#include <QLocale>
#include <QMessageBox>
#include <QTimer>

OptionsDialog::OptionsDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::OptionsDialog),
    model(0),
    mapper(0),
    fProxyIpValid(true)
{
    ui->setupUi(this);
    GUIUtil::restoreWindowGeometry("nOptionsDialogWindow", this->size(), this);

    /* Main elements init */
    ui->databaseCache->setMinimum(nMinDbCache);
    ui->databaseCache->setMaximum(nMaxDbCache);
    ui->threadsScriptVerif->setMinimum(-(int)boost::thread::hardware_concurrency());
    ui->threadsScriptVerif->setMaximum(MAX_SCRIPTCHECK_THREADS);

    /* Network elements init */
#ifndef USE_UPNP
    ui->mapPortUpnp->setEnabled(false);
#endif

    ui->proxyIp->setEnabled(false);
    ui->proxyPort->setEnabled(false);
    ui->proxyPort->setValidator(new QIntValidator(1, 65535, this));

    /** SOCKS version is only selectable for default proxy and is always 5 for IPv6 and Tor */
    ui->socksVersion->setEnabled(false);
    ui->socksVersion->addItem("5", 5);
    ui->socksVersion->addItem("4", 4);
    ui->socksVersion->setCurrentIndex(0);

    connect(ui->connectSocks, SIGNAL(toggled(bool)), ui->proxyIp, SLOT(setEnabled(bool)));
    connect(ui->connectSocks, SIGNAL(toggled(bool)), ui->proxyPort, SLOT(setEnabled(bool)));
    connect(ui->connectSocks, SIGNAL(toggled(bool)), ui->socksVersion, SLOT(setEnabled(bool)));

    ui->proxyIp->installEventFilter(this);

    /* Window elements init */
#ifdef Q_OS_MAC
    /* remove Window tab on Mac */
    ui->tabWidget->removeTab(ui->tabWidget->indexOf(ui->tabWindow));
#endif

    /* Display elements init */
    QDir translations(":translations");
    ui->lang->addItem(QString("(") + tr("default") + QString(")"), QVariant(""));
    foreach(const QString &langStr, translations.entryList())
    {
        QLocale locale(langStr);

        /** check if the locale name consists of 2 parts (language_country) */
        if(langStr.contains("_"))
        {
#if QT_VERSION >= 0x040800
            /** display language strings as "native language - native country (locale name)", e.g. "Deutsch - Deutschland (de)" */
            ui->lang->addItem(locale.nativeLanguageName() + QString(" - ") + locale.nativeCountryName() + QString(" (") + langStr + QString(")"), QVariant(langStr));
#else
            /** display language strings as "language - country (locale name)", e.g. "German - Germany (de)" */
            ui->lang->addItem(QLocale::languageToString(locale.language()) + QString(" - ") + QLocale::countryToString(locale.country()) + QString(" (") + langStr + QString(")"), QVariant(langStr));
#endif
        }
        else
        {
#if QT_VERSION >= 0x040800
            /** display language strings as "native language (locale name)", e.g. "Deutsch (de)" */
            ui->lang->addItem(locale.nativeLanguageName() + QString(" (") + langStr + QString(")"), QVariant(langStr));
#else
            /** display language strings as "language (locale name)", e.g. "German (de)" */
            ui->lang->addItem(QLocale::languageToString(locale.language()) + QString(" (") + langStr + QString(")"), QVariant(langStr));
#endif
        }
    }
#if QT_VERSION >= 0x040700
    ui->thirdPartyTxUrls->setPlaceholderText("https://example.com/tx/%s");
#endif

    ui->unit->setModel(new BitcoinUnits(this));
    ui->transactionFee->setSingleStep(CTransaction::nMinTxFee);
    /* Feature 3 - Recurrent Payment */
    ui->recurrentTransactionAmount->setSingleStep(CTransaction::nMinTxFee);

    /* Widget-to-option mapper */
    mapper = new MonitoredDataMapper(this);
    mapper->setSubmitPolicy(QDataWidgetMapper::ManualSubmit);
    mapper->setOrientation(Qt::Vertical);

    /* setup/change UI elements when proxy IP is invalid/valid */
    connect(this, SIGNAL(proxyIpChecks(QValidatedLineEdit *, int)), this, SLOT(doProxyIpChecks(QValidatedLineEdit *, int)));

    /* Feature 3 - Recurrent Pattern */
    ui->recurrentMonthly_value->setDisplayFormat("dd");
    ui->recurrentMonthly_value2->setDisplayFormat("hh:mm AP");
    ui->recurrentWeekly_value->setDisplayFormat("ddd");
    ui->recurrentWeekly_value->setMinimumDate(QDate::fromString("20000103","yyyyMMdd"));
    ui->recurrentWeekly_value->setMaximumDate(QDate::fromString("20000109","yyyyMMdd"));
    ui->recurrentWeekly_value2->setDisplayFormat("hh:mm AP");
    ui->recurrentDaily_value->setDisplayFormat("hh:mm AP");

    connect(ui->monthlyRecurrent, SIGNAL(toggled(bool)), this, SLOT(monthlyRecurrentToggled(bool)));
    connect(ui->weeklyRecurrent, SIGNAL(toggled(bool)), this, SLOT(weeklyRecurrentToggled(bool)));
    connect(ui->dailyRecurrent, SIGNAL(toggled(bool)), this, SLOT(dailyRecurrentToggled(bool)));

    ui->monthlyRecurrent->setChecked(false);
    ui->weeklyRecurrent->setChecked(false);
    ui->dailyRecurrent->setChecked(false);

    ui->recurrentMonthly_value->setEnabled(false);
    ui->recurrentMonthly_value2->setEnabled(false);
    ui->recurrentWeekly_value->setEnabled(false);
    ui->recurrentWeekly_value2->setEnabled(false);
    ui->recurrentDaily_value->setEnabled(false);

    ui->recurrentPaymentTo_value->setPlaceholderText("The address to send the payment to (e.g. DJ7zB7c5BsB9UJLy1rKQtY7c6CQfGiaRLM)");
    ui->recurrentPaymentLabel_value->setPlaceholderText("Enter a label for this address to add it to the list of used addresses");
}

/* Feature 3 - Recurrent Payment */
void OptionsDialog::monthlyRecurrentToggled(bool input)
{
    if(input)
    {
        ui->weeklyRecurrent->setChecked(false);
        ui->dailyRecurrent->setChecked(false);

        ui->recurrentMonthly_value->setEnabled(true);
        ui->recurrentMonthly_value2->setEnabled(true);
        ui->recurrentWeekly_value->setEnabled(false);
        ui->recurrentWeekly_value2->setEnabled(false);
        ui->recurrentDaily_value->setEnabled(false);

    }
    else
    {
        ui->recurrentMonthly_value->setEnabled(false);
        ui->recurrentMonthly_value2->setEnabled(false);
        ui->recurrentWeekly_value->setEnabled(false);
        ui->recurrentWeekly_value2->setEnabled(false);
        ui->recurrentDaily_value->setEnabled(false);
    }
}
void OptionsDialog::weeklyRecurrentToggled(bool input)
{
    if(input)
    {
        ui->monthlyRecurrent->setChecked(false);
        ui->dailyRecurrent->setChecked(false);

        ui->recurrentMonthly_value->setEnabled(false);
        ui->recurrentMonthly_value2->setEnabled(false);
        ui->recurrentWeekly_value->setEnabled(true);
        ui->recurrentWeekly_value2->setEnabled(true);
        ui->recurrentDaily_value->setEnabled(false);
    }
    else
    {
        ui->recurrentMonthly_value->setEnabled(false);
        ui->recurrentMonthly_value2->setEnabled(false);
        ui->recurrentWeekly_value->setEnabled(false);
        ui->recurrentWeekly_value2->setEnabled(false);
        ui->recurrentDaily_value->setEnabled(false);
    }
}
void OptionsDialog::dailyRecurrentToggled(bool input)
{
    if(input)
    {
        ui->monthlyRecurrent->setChecked(false);
        ui->weeklyRecurrent->setChecked(false);

        ui->recurrentMonthly_value->setEnabled(false);
        ui->recurrentMonthly_value2->setEnabled(false);
        ui->recurrentWeekly_value->setEnabled(false);
        ui->recurrentWeekly_value2->setEnabled(false);
        ui->recurrentDaily_value->setEnabled(true);
    }
    else
    {
        ui->recurrentMonthly_value->setEnabled(false);
        ui->recurrentMonthly_value2->setEnabled(false);
        ui->recurrentWeekly_value->setEnabled(false);
        ui->recurrentWeekly_value2->setEnabled(false);
        ui->recurrentDaily_value->setEnabled(false);
    }
}

OptionsDialog::~OptionsDialog()
{
    GUIUtil::saveWindowGeometry("nOptionsDialogWindow", this);
    delete ui;
}

void OptionsDialog::setModel(OptionsModel *model)
{
    this->model = model;

    if(model)
    {
        /* check if client restart is needed and show persistent message */
        if (model->isRestartRequired())
            showRestartWarning(true);

        QString strLabel = model->getOverriddenByCommandLine();
        if (strLabel.isEmpty())
            strLabel = tr("none");
        ui->overriddenByCommandLineLabel->setText(strLabel);

        connect(model, SIGNAL(displayUnitChanged(int)), this, SLOT(updateDisplayUnit()));

        mapper->setModel(model);
        setMapper();
        mapper->toFirst();
    }

    /* update the display unit, to not use the default ("DOGE") */
    updateDisplayUnit();

    /* warn when one of the following settings changes by user action (placed here so init via mapper doesn't trigger them) */

    /* Main */
    connect(ui->databaseCache, SIGNAL(valueChanged(int)), this, SLOT(showRestartWarning()));
    connect(ui->threadsScriptVerif, SIGNAL(valueChanged(int)), this, SLOT(showRestartWarning()));
    /* Wallet */
    connect(ui->spendZeroConfChange, SIGNAL(clicked(bool)), this, SLOT(showRestartWarning()));
    /* Network */
    connect(ui->connectSocks, SIGNAL(clicked(bool)), this, SLOT(showRestartWarning()));
    /* Display */
    connect(ui->lang, SIGNAL(valueChanged()), this, SLOT(showRestartWarning()));
    connect(ui->thirdPartyTxUrls, SIGNAL(textChanged(const QString &)), this, SLOT(showRestartWarning()));
}

void OptionsDialog::setMapper()
{
    /* Main */
    mapper->addMapping(ui->bitcoinAtStartup, OptionsModel::StartAtStartup);
    mapper->addMapping(ui->threadsScriptVerif, OptionsModel::ThreadsScriptVerif);
    mapper->addMapping(ui->databaseCache, OptionsModel::DatabaseCache);

    /* Wallet */
    mapper->addMapping(ui->transactionFee, OptionsModel::Fee);
    mapper->addMapping(ui->spendZeroConfChange, OptionsModel::SpendZeroConfChange);
    mapper->addMapping(ui->coinControlFeatures, OptionsModel::CoinControlFeatures);

    /* Network */
    mapper->addMapping(ui->mapPortUpnp, OptionsModel::MapPortUPnP);

    mapper->addMapping(ui->connectSocks, OptionsModel::ProxyUse);
    mapper->addMapping(ui->proxyIp, OptionsModel::ProxyIP);
    mapper->addMapping(ui->proxyPort, OptionsModel::ProxyPort);
    mapper->addMapping(ui->socksVersion, OptionsModel::ProxySocksVersion);

    /* Window */
#ifndef Q_OS_MAC
    mapper->addMapping(ui->minimizeToTray, OptionsModel::MinimizeToTray);
    mapper->addMapping(ui->minimizeOnClose, OptionsModel::MinimizeOnClose);
#endif

    /* Display */
    mapper->addMapping(ui->lang, OptionsModel::Language);
    mapper->addMapping(ui->unit, OptionsModel::DisplayUnit);
    mapper->addMapping(ui->displayAddresses, OptionsModel::DisplayAddresses);
    mapper->addMapping(ui->thirdPartyTxUrls, OptionsModel::ThirdPartyTxUrls);

    /* Feature 3 - Recurrent Payment */
    mapper->addMapping(ui->monthlyRecurrent, OptionsModel::IsMonthlyRecurrent);
    mapper->addMapping(ui->weeklyRecurrent, OptionsModel::IsWeeklyRecurrent);
    mapper->addMapping(ui->dailyRecurrent, OptionsModel::IsDailyRecurrent);
    mapper->addMapping(ui->recurrentMonthly_value, OptionsModel::RecurrentMonthlyDate);
    mapper->addMapping(ui->recurrentMonthly_value2, OptionsModel::RecurrentMonthlyTime);
    mapper->addMapping(ui->recurrentWeekly_value, OptionsModel::RecurrentWeeklyDate);
    mapper->addMapping(ui->recurrentWeekly_value2, OptionsModel::RecurrentWeeklyTime);
    mapper->addMapping(ui->recurrentDaily_value, OptionsModel::RecurrentDailyTime);

    mapper->addMapping(ui->recurrentPaymentTo_value, OptionsModel::RecurrentPaymentAddress);
    mapper->addMapping(ui->recurrentPaymentLabel_value, OptionsModel::RecurrentPaymentLabel);
    mapper->addMapping(ui->recurrentTransactionAmount, OptionsModel::RecurrentPaymentAmount);
}

void OptionsDialog::enableOkButton()
{
    /* prevent enabling of the OK button when data modified, if there is an invalid proxy address present */
    if(fProxyIpValid)
        setOkButtonState(true);
}

void OptionsDialog::disableOkButton()
{
    setOkButtonState(false);
}

void OptionsDialog::setOkButtonState(bool fState)
{
    ui->okButton->setEnabled(fState);
}

void OptionsDialog::on_resetButton_clicked()
{
    if(model)
    {
        // confirmation dialog
        QMessageBox::StandardButton btnRetVal = QMessageBox::question(this, tr("Confirm options reset"),
            tr("Client restart required to activate changes.") + "<br><br>" + tr("Client will be shutdown, do you want to proceed?"),
            QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Cancel);

        if(btnRetVal == QMessageBox::Cancel)
            return;

        /* reset all options and close GUI */
        model->Reset();
        QApplication::quit();
    }
}

void OptionsDialog::on_okButton_clicked()
{
    mapper->submit();
    accept();
}

void OptionsDialog::on_cancelButton_clicked()
{
    reject();
}

void OptionsDialog::showRestartWarning(bool fPersistent)
{
    ui->statusLabel->setStyleSheet("QLabel { color: red; }");

    if(fPersistent)
    {
        ui->statusLabel->setText(tr("Client restart required to activate changes."));
    }
    else
    {
        ui->statusLabel->setText(tr("This change would require a client restart."));
        // clear non-persistent status label after 10 seconds
        // Todo: should perhaps be a class attribute, if we extend the use of statusLabel
        QTimer::singleShot(10000, this, SLOT(clearStatusLabel()));
    }
}

void OptionsDialog::clearStatusLabel()
{
    ui->statusLabel->clear();
}

void OptionsDialog::updateDisplayUnit()
{
    if(model)
    {
        /* Update transactionFee with the current unit */
        ui->transactionFee->setDisplayUnit(model->getDisplayUnit());

        /* Feature 3 - Recurrent Payment */
        ui->recurrentTransactionAmount->setDisplayUnit(model->getDisplayUnit());        
    }
}

void OptionsDialog::doProxyIpChecks(QValidatedLineEdit *pUiProxyIp, int nProxyPort)
{
    Q_UNUSED(nProxyPort);

    const std::string strAddrProxy = pUiProxyIp->text().toStdString();
    CService addrProxy;

    /* Check for a valid IPv4 / IPv6 address */
    if (!(fProxyIpValid = LookupNumeric(strAddrProxy.c_str(), addrProxy)))
    {
        disableOkButton();
        pUiProxyIp->setValid(false);
        ui->statusLabel->setStyleSheet("QLabel { color: red; }");
        ui->statusLabel->setText(tr("The supplied proxy address is invalid."));
    }
    else
    {
        enableOkButton();
        ui->statusLabel->clear();
    }
}

bool OptionsDialog::eventFilter(QObject *object, QEvent *event)
{
    if(event->type() == QEvent::FocusOut)
    {
        if(object == ui->proxyIp)
        {
            emit proxyIpChecks(ui->proxyIp, ui->proxyPort->text().toInt());
        }
    }
    return QDialog::eventFilter(object, event);
}
