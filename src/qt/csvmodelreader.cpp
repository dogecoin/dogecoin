// Copyright (c) 2011-2013 The Bitcoin developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

 /* Feature 5 : this class is added as for the implmentation of feature 5
    It will read a csv file containig bitcoin wallets address and add 
    them to the send addresses.         */
    
#include "csvmodelreader.h"
#include <QAbstractItemModel>
#include <QMessageBox>
#include <QString>
#include <QStringList>
#include <QFile>
#include <QTextStream>

CSVModelReader::CSVModelReader(const QString &filename) :
    filename(filename)
{
}


bool CSVModelReader::read(AddressTableModel *model)
{
    QMessageBox msgBox;
    if(!model)
        return false;

     QFile file(filename);
     QStringList list;

    if(!file.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        return false;
    }
    else
    {
        QTextStream In(&file);
        QString str;
       
        while (!In.atEnd()) 
        {
            str=In.readLine();
            list = str.split(",");
            QString label (list.value(0));
            QString address (list.value(1));

            label.replace(QString("\""), QString(""));
            address.replace(QString("\""), QString(""));


            model->addRow(AddressTableModel::Send,  label, address);
        }   

        file.close();
        return true;
    }

}


QStringList CSVModelReader::read()
{
    QMessageBox msgBox;
  
     QFile file(filename);
     QStringList list;

    if(!file.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        return list;
    }
    else
    {
        QTextStream In(&file);
        QString str;
       
        while (!In.atEnd()) 
        {
            str=In.readLine();
            list.append(str);
        }   
        return list;
    }

}

