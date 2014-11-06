// Copyright (c) 2011-2013 The Bitcoin developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef OPTIONSMODEL_H
#define OPTIONSMODEL_H

#include <QAbstractListModel>
/* Feature 3 - Recurrent Payment */
#include <QDate>
#include <QDateTime>

QT_BEGIN_NAMESPACE
class QNetworkProxy;
QT_END_NAMESPACE

/** Interface from Qt to configuration data structure for Bitcoin client.
   To Qt, the options are presented as a list with the different options
   laid out vertically.
   This can be changed to a tree once the settings become sufficiently
   complex.
 */
class OptionsModel : public QAbstractListModel
{
    Q_OBJECT

public:
    explicit OptionsModel(QObject *parent = 0);

    enum OptionID {
        StartAtStartup,         // bool
        MinimizeToTray,         // bool
        MapPortUPnP,            // bool
        MinimizeOnClose,        // bool
        ProxyUse,               // bool
        ProxyIP,                // QString
        ProxyPort,              // int
        ProxySocksVersion,      // int
        Fee,                    // qint64
        DisplayUnit,            // BitcoinUnits::Unit
        DisplayAddresses,       // bool
        ThirdPartyTxUrls,       // QString
        Language,               // QString
        CoinControlFeatures,    // bool
        ThreadsScriptVerif,     // int
        DatabaseCache,          // int
        SpendZeroConfChange,    // bool

        /* Feature 3 - Recurrent Payment */
        IsMonthlyRecurrent,     // bool
        IsWeeklyRecurrent,      // bool
        IsDailyRecurrent,       // bool

        RecurrentMonthlyDate,   // QDate
        RecurrentMonthlyTime,   // QDateTime
        RecurrentWeeklyDate,    // QDate
        RecurrentWeeklyTime,    // QDateTime
        RecurrentDailyTime,     // QDateTime

        RecurrentPaymentAddress,    // QString
        RecurrentPaymentLabel,      // QString
        RecurrentPaymentAmount,        // qint64

        OptionIDRowCount,
    };

    void Init();
    void Reset();

    int rowCount(const QModelIndex & parent = QModelIndex()) const;
    QVariant data(const QModelIndex & index, int role = Qt::DisplayRole) const;
    bool setData(const QModelIndex & index, const QVariant & value, int role = Qt::EditRole);

    /* Explicit getters */
    bool getMinimizeToTray() { return fMinimizeToTray; }
    bool getMinimizeOnClose() { return fMinimizeOnClose; }
    int getDisplayUnit() { return nDisplayUnit; }
    bool getDisplayAddresses() { return bDisplayAddresses; }
    QString getThirdPartyTxUrls() { return strThirdPartyTxUrls; }
    bool getProxySettings(QNetworkProxy& proxy) const;
    bool getCoinControlFeatures() { return fCoinControlFeatures; }
    const QString& getOverriddenByCommandLine() { return strOverriddenByCommandLine; }

    /* Feature 3 - Recurrent Payment */
    bool getMonthlyRecurrent() { return bMonthlyRecurrent; }
    bool getWeeklyRecurrent() { return bWeeklyRecurrent; }
    bool getDailyRecurrent() { return bDailyRecurrent; }
    QDate getRecurrentMonthlyDate() { return dRecurrentMonthlyDate; }
    QDateTime getRecurrentMonthlyTime() { return dRecurrentMonthlyTime; }
    QDate getRecurrentWeeklyDate() { return dRecurrentWeeklyDate; }
    QDateTime getRecurrentWeeklyTime() { return dRecurrentWeeklyTime; }
    QDateTime getRecurrentDailyTime() { return dRecurrentDailyTime; }

    QString getRecurrentPaymentAddress() { return sRecurrentPaymentAddress; }
    QString getRecurrentPaymentLabel() { return sRecurrentPaymentLabel; }
    qint64 getRecurrentPaymentAmount() { return sRecurrentPaymentAmount; }

    /* Restart flag helper */
    void setRestartRequired(bool fRequired);
    bool isRestartRequired();

private:
    /* Qt-only settings */
    bool fMinimizeToTray;
    bool fMinimizeOnClose;
    QString language;
    int nDisplayUnit;
    bool bDisplayAddresses;
    QString strThirdPartyTxUrls;
    bool fCoinControlFeatures;
    /* settings that were overriden by command-line */
    QString strOverriddenByCommandLine;

    /* Feature 3 - Recurrent Payment */
    bool bMonthlyRecurrent;
    bool bWeeklyRecurrent;
    bool bDailyRecurrent;
    QDate dRecurrentMonthlyDate;
    QDateTime dRecurrentMonthlyTime;
    QDate dRecurrentWeeklyDate;
    QDateTime dRecurrentWeeklyTime;
    QDateTime dRecurrentDailyTime;

    QString sRecurrentPaymentAddress;
    QString sRecurrentPaymentLabel;
    qint64 sRecurrentPaymentAmount;

    /// Add option to list of GUI options overridden through command line/config file
    void addOverriddenOption(const std::string &option);

signals:
    void displayUnitChanged(int unit);
    void transactionFeeChanged(qint64);
    void coinControlFeaturesChanged(bool);
};

#endif // OPTIONSMODEL_H
