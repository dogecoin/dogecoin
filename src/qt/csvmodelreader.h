// Copyright (c) 2011-2013 The Bitcoin developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef CSVMODELREADER_H
#define CSVMODELREADER_H

#include <QList>
#include <QObject>

/* Feature 5 - Incldues*/
#include "addresstablemodel.h"

/* Feature 5 - End*/

QT_BEGIN_NAMESPACE
class QAbstractItemModel;
class AddressTableModel;
QT_END_NAMESPACE

/** Feature 5 : Import a Qt table model from a CSV file. This is useful for having a portable 
	addressbook for bitcoin receivers
 */
class CSVModelReader : public QObject
{
    Q_OBJECT

public:
    explicit CSVModelReader(const QString &filename);

    bool read(AddressTableModel *model);
    QStringList read();

private:
    QString filename;

};

#endif // CSVMODELREADER