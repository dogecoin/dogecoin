// Copyright (c) 2013 The Bitcoin Core developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

//
// Unit tests for alert system
//

#include "alert.h"
#include "data/alertTests.raw.h"

#include "key.h"
#include "serialize.h"
#include "util.h"
#include "version.h"

#include <fstream>

#include <boost/filesystem/operations.hpp>
#include <boost/foreach.hpp>
#include <boost/test/unit_test.hpp>

/*this test relies on signed alerts in src/test/data/alertTests.raw

how to create this data:
1. set secretKey below to the secret key belonging to the mainnet vAlertPubKey from chainparams.cpp
2. delete the file src/test/alertTests.raw if it exists
3. compile the test suite with CREATE_ALERTS defined
4. execute this test suite
5. move the now created file src/test/data/alertTests.raw to src/test/data/alertTests.raw
6. undefine CREATE_ALERTS and compile the test suite again
*/

#ifdef CREATE_ALERTS
struct CreateAlerts
{
    void SignAndSave(CAlert alert, const std::string filename) {
        //serialze alert message
        CDataStream sMsg(SER_NETWORK, PROTOCOL_VERSION);
        sMsg << (CUnsignedAlert)alert;
        alert.vchMsg.reserve(sMsg.size());
        for(size_t i=0; i<sMsg.size(); i++) {
            alert.vchMsg.push_back(sMsg[i]);
        }

        //a dummy secret key with the public key
        //0469204F0E1800E16C1F85176BDC27A245F09987DB71A1EF5C4BD48A42F9AFD1D74F21469488DB552B594AC29CE667AD60DAAD0FFBCE03FB0C2AC49FFB07B36DC5
        //set to match the mainnet vAlertPubKey from chainparams.cpp
        const std::vector<unsigned char> secretKey = ParseHex("1A4976EE6174D80B8B3FF5E9B9C6E638CABFA9A5975557FEA967BC089A5584EE");
        CKey secret;
        secret.Set(secretKey.begin(), secretKey.end(), false);
        assert(secret.IsValid());
        
        //sign alert
        secret.Sign(alert.GetHash(), alert.vchSig);
        assert(alert.CheckSignature());
        
        //serialize alert
        CDataStream ss(SER_DISK, CLIENT_VERSION);
        ss << alert;
        
        //write alert
        std::ofstream fs;
        fs.open(filename.c_str(), std::ios::out | std::ios::app | std::ios::binary);
        fs.write((char*)&ss[0], ss.size());
        fs.close();
    }

    //
    // alertTests contains 7 alerts, generated with this code:
    //
    CreateAlerts()
    {
        std::string filename("src/test/alertTests.raw");

        CAlert alert;
        alert.nRelayUntil   = 60;
        alert.nExpiration   = 24 * 60 * 60;
        alert.nID           = 1;
        alert.nCancel       = 0;   // cancels previous messages up to this ID number
        alert.nMinVer       = 0;  // These versions are protocol versions
        alert.nMaxVer       = 999001;
        alert.nPriority     = 1;
        alert.strComment    = "Alert comment";
        alert.strStatusBar  = "Alert 1";

        SignAndSave(alert, filename);

        alert.setSubVer.insert(std::string("/Satoshi:0.1.0/"));
        alert.strStatusBar  = "Alert 1 for Satoshi 0.1.0";
        SignAndSave(alert, filename);

        alert.setSubVer.insert(std::string("/Satoshi:0.2.0/"));
        alert.strStatusBar  = "Alert 1 for Satoshi 0.1.0, 0.2.0";
        SignAndSave(alert, filename);

        alert.setSubVer.clear();
        ++alert.nID;
        alert.nCancel = 1;
        alert.nPriority = 100;
        alert.strStatusBar  = "Alert 2, cancels 1";
        SignAndSave(alert, filename);

        alert.nExpiration += 60;
        ++alert.nID;
        SignAndSave(alert, filename);

        ++alert.nID;
        alert.nMinVer = 11;
        alert.nMaxVer = 22;
        SignAndSave(alert, filename);

        ++alert.nID;
        alert.strStatusBar  = "Alert 2 for Satoshi 0.1.0";
        alert.setSubVer.insert(std::string("/Satoshi:0.1.0/"));
        SignAndSave(alert, filename);

        ++alert.nID;
        alert.nMinVer = 0;
        alert.nMaxVer = 999999;
        alert.strStatusBar  = "Evil Alert'; /bin/ls; echo '";
        alert.setSubVer.clear();
        SignAndSave(alert, filename);
    }
    ~CreateAlerts() { }
};

BOOST_GLOBAL_FIXTURE(CreateAlerts)
#endif

struct ReadAlerts
{
    ReadAlerts()
    {
        std::vector<unsigned char> vch(alert_tests::alertTests, alert_tests::alertTests + sizeof(alert_tests::alertTests));
        CDataStream stream(vch, SER_DISK, CLIENT_VERSION);
        try {
            while (stream.good())
            {
                CAlert alert;
                stream >> alert;
                alerts.push_back(alert);
            }
        }
        catch (std::exception) { }
    }
    ~ReadAlerts() { }

    static std::vector<std::string> read_lines(boost::filesystem::path filepath)
    {
        std::vector<std::string> result;

        std::ifstream f(filepath.string().c_str());
        std::string line;
        while (std::getline(f,line))
            result.push_back(line);

        return result;
    }

    std::vector<CAlert> alerts;
};

BOOST_FIXTURE_TEST_SUITE(Alert_tests, ReadAlerts)


BOOST_AUTO_TEST_CASE(AlertApplies)
{
    SetMockTime(11);

    BOOST_FOREACH(const CAlert& alert, alerts)
    {
        BOOST_CHECK(alert.CheckSignature());
    }

    BOOST_CHECK(alerts.size() >= 3);

    // Matches:
    BOOST_CHECK(alerts[0].AppliesTo(1, ""));
    BOOST_CHECK(alerts[0].AppliesTo(999001, ""));
    BOOST_CHECK(alerts[0].AppliesTo(1, "/Satoshi:11.11.11/"));

    BOOST_CHECK(alerts[1].AppliesTo(1, "/Satoshi:0.1.0/"));
    BOOST_CHECK(alerts[1].AppliesTo(999001, "/Satoshi:0.1.0/"));

    BOOST_CHECK(alerts[2].AppliesTo(1, "/Satoshi:0.1.0/"));
    BOOST_CHECK(alerts[2].AppliesTo(1, "/Satoshi:0.2.0/"));

    // Don't match:
    BOOST_CHECK(!alerts[0].AppliesTo(-1, ""));
    BOOST_CHECK(!alerts[0].AppliesTo(999002, ""));

    BOOST_CHECK(!alerts[1].AppliesTo(1, ""));
    BOOST_CHECK(!alerts[1].AppliesTo(1, "Satoshi:0.1.0"));
    BOOST_CHECK(!alerts[1].AppliesTo(1, "/Satoshi:0.1.0"));
    BOOST_CHECK(!alerts[1].AppliesTo(1, "Satoshi:0.1.0/"));
    BOOST_CHECK(!alerts[1].AppliesTo(-1, "/Satoshi:0.1.0/"));
    BOOST_CHECK(!alerts[1].AppliesTo(999002, "/Satoshi:0.1.0/"));
    BOOST_CHECK(!alerts[1].AppliesTo(1, "/Satoshi:0.2.0/"));

    BOOST_CHECK(!alerts[2].AppliesTo(1, "/Satoshi:0.3.0/"));

    SetMockTime(0);
}


// This uses sh 'echo' to test the -alertnotify function, writing to a
// /tmp file. So skip it on Windows:
#ifndef WIN32
BOOST_AUTO_TEST_CASE(AlertNotify)
{
    SetMockTime(11);

    boost::filesystem::path temp = GetTempPath() / "alertnotify.txt";
    boost::filesystem::remove(temp);

    mapArgs["-alertnotify"] = std::string("echo %s >> ") + temp.string();

    BOOST_FOREACH(CAlert alert, alerts)
        alert.ProcessAlert(false);

    std::vector<std::string> r = read_lines(temp);
    BOOST_CHECK_EQUAL(r.size(), 4u);
    BOOST_CHECK_EQUAL(r[0], "Alert 1");
    BOOST_CHECK_EQUAL(r[1], "Alert 2, cancels 1");
    BOOST_CHECK_EQUAL(r[2], "Alert 2, cancels 1");
    BOOST_CHECK_EQUAL(r[3], "Evil Alert; /bin/ls; echo "); // single-quotes should be removed

    boost::filesystem::remove(temp);

    SetMockTime(0);
}
#endif

BOOST_AUTO_TEST_SUITE_END()
