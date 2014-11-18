#include "walletBackupTests.h"

#include "optionsmodel.h"
#include "walletview.h"
#include <iostream>

#include <openssl/x509.h>
#include <openssl/x509_vfy.h>

#include <QFileOpenEvent>
#include <QTemporaryFile>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>



 /* Feature 1 - Testing Bakup on startup if this option is enabled*/
void WalletBackupTests::startupBackupTests()
{


    OptionsModel *optionsModel;
    optionsModel = new OptionsModel();

    bool bBackupOnDemandOpt = false;
    QString sBackupFileLocation = "";

    bBackupOnDemandOpt = optionsModel->getBackupOnStartOpt();
    if (bBackupOnDemandOpt)
    {
        sBackupFileLocation =  optionsModel->getBackupFileLocation();

      if (sBackupFileLocation == "")
       QFAIL("Backup file location is not found in the configuration file");

//toLocal8Bit().data()
   //boost::filesystem::path path(sBackupFileLocation);
     QString path = sBackupFileLocation;

    // if(!boost::filesystem::is_directory(sBackupFileLocation.toLocal8Bit().data()))
    // {
    //   QFAIL("Backup file location is not found in the configuration file");
    // }
    if(boost::filesystem::exists(sBackupFileLocation.toLocal8Bit().data()))
  {  
      QFAIL("Backup file location is not found in the configuration file");
    }
    //QFAIL("Backup file is not created at Dogecoin startup");
    }
  
}


