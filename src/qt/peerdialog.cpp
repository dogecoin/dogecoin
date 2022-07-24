// Copyright (c) 2011-2016 The Bitcoin Core developers
// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "guiutil.h"
#if defined(HAVE_CONFIG_H)
#include "config/bitcoin-config.h"
#endif

#include <iostream>
#include <string>

#include "peerdialog.h"

#include "ui_addpeerdialog.h"
#include "ui_testpeerdialog.h"

#include "net.h"
#include "net_processing.h"
#include "netbase.h"
#include "protocol.h"
#include "chainparams.h"
#include "util.h"

#include <stdio.h>

#include <QMessageBox>
#include <QHostAddress>
#include <QAbstractSocket>
#include <QUrl>

/** Function to manage peers */
QString PeerTools::ManagePeer(QString type, QString peer)
{
    std::string peerAddress = peer.toStdString();

    if (peerAddress.size() > 256) {
      return tr("Error: Node address is invalid");
    }

    if(!g_connman)
        return tr("Error: Peer-to-peer functionality missing or disabled");

    if (type == "onetry")
    {
        CAddress addr;
        g_connman->OpenNetworkConnection(addr, false, NULL, peerAddress.c_str());
        return tr("Attempted to one try node.");
    }

    if (type == "add")
    {
        if(!g_connman->AddNode(peerAddress))
            return tr("Error: Unable to add node");
    }
    else if(type == "remove")
    {
        if(!g_connman->RemoveAddedNode(peerAddress))
        {
            if(!g_connman->DisconnectNode(peerAddress))
                return tr("Node not found in connected nodes");

            return tr("Disconnected the node: ") + peer;
        }
        else
        {
            if(!g_connman->DisconnectNode(peerAddress))
                return tr("Node not found in connected nodes");
        }
    }

    return tr("Returned OK.");
}

/** Check if Peer is valid */
bool PeerTools::CheckPeerAddress(QString address)
{
    CNetAddr addr;
    return LookupHost(address.toStdString().c_str(), addr, true);
}

/** Get port based on current chain */
QString PeerTools::GetPort()
{
    return QString::number(Params().GetDefaultPort());
}

/** Add Peer Dialog */
AddPeerDialog::AddPeerDialog(QWidget *parent) : 
    QWidget(parent),
    ui(new Ui::AddPeerDialog)
{
    ui->setupUi(this);

    ui->peerPort->setValidator( new QIntValidator(1, 65535, this) );

    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(on_addPeerClicked()));
}

AddPeerDialog::~AddPeerDialog()
{
    delete ui;
}

void AddPeerDialog::on_addPeerClicked()
{
    QString address = ui->peerAddress->text();
    QString port = ui->peerPort->text();
    QString data = "";

    if(address.isEmpty()) 
    {
        QMessageBox::critical(this, tr("Add Peer"), tr("Please enter an address."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    if(port.isEmpty()) 
    {
        port = PeerTools::GetPort();
        ui->peerPort->setText(port);
    }

    if(!PeerTools::CheckPeerAddress(address))
    {
        QMessageBox::critical(this, tr("Add Peer"), tr("Please enter a valid peer address."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    data = address + ":" + port;

    if(QMessageBox::Ok == QMessageBox::information(this, tr("Add Peer"), PeerTools::ManagePeer("add", data), QMessageBox::Ok, QMessageBox::Ok))
        this->close();
}

/** Add Test Peer Dialog */
TestPeerDialog::TestPeerDialog(QWidget *parent) : 
    QWidget(parent),
    ui(new Ui::TestPeerDialog)
{
    ui->setupUi(this);

    ui->peerPort->setValidator( new QIntValidator(1, 65535, this) );

    connect(ui->pushButton, SIGNAL(clicked()), this, SLOT(on_testPeerClicked()));
}

TestPeerDialog::~TestPeerDialog()
{
    delete ui;
}

void TestPeerDialog::on_testPeerClicked()
{
    QString address = ui->peerAddress->text();
    QString port = ui->peerPort->text();
    QString data = "";

    if(address.isEmpty()) 
    {
        QMessageBox::critical(this, tr("Test Peer"), tr("Please enter an address."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    if(port.isEmpty()) 
    {
        port = PeerTools::GetPort();
        ui->peerPort->setText(port);
    }

    if(!PeerTools::CheckPeerAddress(address))
    {
        QMessageBox::critical(this, tr("Test Peer"), tr("Please enter a valid peer address."), QMessageBox::Ok, QMessageBox::Ok);
        return;
    }

    data = address + ":" + port;

    if(QMessageBox::Ok == QMessageBox::information(this, tr("Try Peer"), PeerTools::ManagePeer("onetry", data), QMessageBox::Ok, QMessageBox::Ok))
        this->close();
}
