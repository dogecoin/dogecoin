// Copyright (c) 2011-2013 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "bitcoinunits.h"

#include "primitives/transaction.h"

#include <QStringList>
#include <QLocale>

BitcoinUnits::BitcoinUnits(QObject *parent):
        QAbstractListModel(parent),
        unitlist(availableUnits())
{
}

QList<BitcoinUnits::Unit> BitcoinUnits::availableUnits()
{
    QList<BitcoinUnits::Unit> unitlist;
    unitlist.append(BTC);
    unitlist.append(kBTC);
    unitlist.append(MBTC);
    //unitlist.append(mBTC);
    //unitlist.append(uBTC);
    return unitlist;
}

bool BitcoinUnits::valid(int unit)
{
    switch(unit)
    {
    case MBTC:
    case kBTC:
    case BTC:
        return true;
    case mBTC:
    case uBTC:
    default:
        return false;
    }
}

QString BitcoinUnits::name(int unit)
{
    switch(unit)
    {
    case MBTC: return QString("MDOGE");
    case kBTC: return QString("kDOGE");
    case BTC: return QString("DOGE");
    case mBTC: return QString("mDOGE");
    case uBTC: return QChar(0x03BC) + QString("DOGE");
    default: return QString("???");
    }
}

QString BitcoinUnits::description(int unit)
{
    switch(unit)
    {
    case MBTC: return tr("Mega-Dogecoins (1,000,000)");
    case kBTC: return tr("Kilo-Dogecoins (1,000)");
    case BTC: return tr("Dogecoins");
    case mBTC: return tr("Milli-Dogecoins (1 / 1,000)");
    case uBTC: return tr("Micro-Dogecoins (1 / 1,000,000)");
    default: return QString("???");
    }
}

qint64 BitcoinUnits::factor(int unit)
{
    switch(unit)
    {
    case MBTC: return 100000000000000;
    case kBTC: return 100000000000;
    case BTC:  return 100000000;
    case mBTC: return 100000;
    case uBTC: return 100;
    default:   return 100000000;
    }
}

qint64 BitcoinUnits::maxAmount(int unit)
{
    switch(unit)
    {
    case MBTC: return Q_INT64_C(900000);
    case kBTC: return Q_INT64_C(900000000);
    case BTC:  return Q_INT64_C(900000000000);    //less than the coin supply until the year 2170
    case mBTC: return Q_INT64_C(900000000000000);
    case uBTC: return Q_INT64_C(900000000000000000); // Slightly under max value for int64
    default:   return 0;
    }
}

int BitcoinUnits::decimals(int unit)
{
    switch(unit)
    {
    case MBTC: return 14;
    case kBTC: return 11;
    case BTC: return 8;
    case mBTC: return 5;
    case uBTC: return 2;
    default: return 0;
    }
}

QString BitcoinUnits::format(int unit, const CAmount& nIn, bool fPlus, bool fTrim, const QLocale &locale_in)
{
    // Note: not using straight sprintf here because we do NOT want
    // localized number formatting.
    if(!valid(unit))
        return QString(); // Refuse to format invalid unit
    qint64 n = (qint64)nIn;
    QLocale locale(locale_in);
    qint64 coin = factor(unit);
    int num_decimals = decimals(unit);

    qint64 n_abs = (n > 0 ? n : -n);
    qint64 quotient = n_abs / coin;
    qint64 remainder = n_abs % coin;
    // Quotient has group (decimal) separators if locale has this enabled
    QString quotient_str = locale.toString(quotient);
    // Remainder does not have group separators
    locale.setNumberOptions(QLocale::OmitGroupSeparator | QLocale::RejectGroupSeparator);
    QString remainder_str = locale.toString(remainder).rightJustified(num_decimals, '0');

    if(fTrim)
    {
        // Right-trim excess zeros after the decimal point
        int nTrim = 0;
        for (int i = remainder_str.size()-1; i>=2 && (remainder_str.at(i) == '0'); --i)
            ++nTrim;
        remainder_str.chop(nTrim);
    }

    if (n < 0)
        quotient_str.insert(0, '-');
    else if (fPlus && n > 0)
        quotient_str.insert(0, '+');
    return quotient_str + locale.decimalPoint() + remainder_str;
}

// TODO: Review all remaining calls to BitcoinUnits::formatWithUnit to
// TODO: determine whether the output is used in a plain text context
// TODO: or an HTML context (and replace with
// TODO: BtcoinUnits::formatHtmlWithUnit in the latter case). Hopefully
// TODO: there aren't instances where the result could be used in
// TODO: either context.

// NOTE: Using formatWithUnit in an HTML context risks wrapping
// quantities at the thousands separator. More subtly, it also results
// in a standard space rather than a thin space, due to a bug in Qt's
// XML whitespace canonicalisation
//
// Please take care to use formatHtmlWithUnit instead, when
// appropriate.

QString BitcoinUnits::formatWithUnit(int unit, const CAmount& amount, bool plussign, bool trim, const QLocale &locale)
{
    return format(unit, amount, plussign, trim) + QString(" ") + name(unit);
}

QString BitcoinUnits::formatHtmlWithUnit(int unit, const CAmount& amount, bool plussign, bool trim, const QLocale &locale)
{
    QString str(formatWithUnit(unit, amount, plussign, trim, locale));
    str.replace(QChar(THIN_SP_CP), QString(THIN_SP_HTML));
    return QString("<span style='white-space: nowrap;'>%1</span>").arg(str);
}


bool BitcoinUnits::parse(int unit, const QString &value, CAmount *val_out, const QLocale &locale_in)
{
    if(!valid(unit) || value.isEmpty())
        return false; // Refuse to parse invalid unit or empty string

    QLocale locale(locale_in);
    qint64 coin = factor(unit);
    int num_decimals = decimals(unit);

    QStringList parts = value.split(locale.decimalPoint());
    bool ok = false;

    if(parts.size() > 2)
        return false; // More than one decimal point

    // Parse whole part (may include locale-specific group separators)
#if QT_VERSION < 0x050000
    qint64 whole = locale.toLongLong(parts[0], &ok, 10);
#else
    qint64 whole = locale.toLongLong(parts[0], &ok);
#endif
    if(!ok)
        return false; // Parse error
    if(whole > maxAmount(unit) || whole < 0)
        return false; // Overflow or underflow

    // Parse decimals part (if present, may not include group separators)
    qint64 decimals = 0;
    if(parts.size() > 1)
    {
        if(parts[1].size() > num_decimals)
            return false; // Exceeds max precision
        locale.setNumberOptions(QLocale::OmitGroupSeparator | QLocale::RejectGroupSeparator);
#if QT_VERSION < 0x050000
        decimals = locale.toLongLong(parts[1].leftJustified(num_decimals, '0'), &ok, 10);
#else
        decimals = locale.toLongLong(parts[1].leftJustified(num_decimals, '0'), &ok);
#endif
        if(!ok || decimals < 0)
            return false; // Parse error
    }

    if(val_out)
    {
        *val_out = whole * coin + decimals;
    }
    return ok;
}

QString BitcoinUnits::getAmountColumnTitle(int unit)
{
    QString amountTitle = QObject::tr("Amount");
    if (BitcoinUnits::valid(unit))
    {
        amountTitle += " ("+BitcoinUnits::name(unit) + ")";
    }
    return amountTitle;
}

int BitcoinUnits::rowCount(const QModelIndex &parent) const
{
    Q_UNUSED(parent);
    return unitlist.size();
}

QVariant BitcoinUnits::data(const QModelIndex &index, int role) const
{
    int row = index.row();
    if(row >= 0 && row < unitlist.size())
    {
        Unit unit = unitlist.at(row);
        switch(role)
        {
        case Qt::EditRole:
        case Qt::DisplayRole:
            return QVariant(name(unit));
        case Qt::ToolTipRole:
            return QVariant(description(unit));
        case UnitRole:
            return QVariant(static_cast<int>(unit));
        }
    }
    return QVariant();
}

CAmount BitcoinUnits::maxMoney()
{
    return MAX_MONEY;
}
