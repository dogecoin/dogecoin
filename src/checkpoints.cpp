// Copyright (c) 2009-2014 The Bitcoin developers
// Copyright (c) 2013-2014 The Dogecoin developers
// Copyright (c)      2014 The Inutoshi developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "checkpoints.h"

#include "main.h"
#include "uint256.h"

#include <stdint.h>

#include <boost/assign/list_of.hpp> // for 'map_list_of()'
#include <boost/foreach.hpp>

namespace Checkpoints
{
    typedef std::map<int, uint256> MapCheckpoints;

    // How many times we expect transactions after the last checkpoint to
    // be slower. This number is a compromise, as it can't be accurate for
    // every system. When reindexing from a fast disk with a slow CPU, it
    // can be up to 20, while when downloading from a slow network with a
    // fast multicore CPU, it won't be much higher than 1.
    static const double SIGCHECK_VERIFICATION_FACTOR = 5.0;

    struct CCheckpointData {
        const MapCheckpoints *mapCheckpoints;
        int64_t nTimeLastCheckpoint;
        int64_t nTransactionsLastCheckpoint;
        double fTransactionsPerDay;
    };

    bool fEnabled = true;

    // What makes a good checkpoint block?
    // + Is surrounded by blocks with reasonable timestamps
    //   (no blocks before with a timestamp after, none after with
    //    timestamp before)
    // + Contains no strange transactions
    static MapCheckpoints mapCheckpoints =
        boost::assign::map_list_of
        (     0, uint256("0x1a91e3dace36e2be3bf030a65679fe821aa1d6ef92e7c9902eb318182c355691"))
        ( 42279, uint256("0x8444c3ef39a46222e87584ef956ad2c9ef401578bd8b51e8e4b9a86ec3134d3a"))
        ( 42400, uint256("0x557bb7c17ed9e6d4a6f9361cfddf7c1fc0bdc394af7019167442b41f507252b4"))
        ( 104679, uint256("0x35eb87ae90d44b98898fec8c39577b76cb1eb08e1261cfc10706c8ce9a1d01cf"))
        ( 128370, uint256("0x3f9265c94cab7dc3bd6a2ad2fb26c8845cb41cff437e0a75ae006997b4974be6"))
        ( 145000, uint256("0xcc47cae70d7c5c92828d3214a266331dde59087d4a39071fa76ddfff9b7bde72"))
        ( 165393, uint256("0x7154efb4009e18c1c6a6a79fc6015f48502bcd0a1edd9c20e44cd7cbbe2eeef1"))
        ( 186774, uint256("0x3c712c49b34a5f34d4b963750d6ba02b73e8a938d2ee415dcda141d89f5cb23a"))
        ( 199992, uint256("0x3408ff829b7104eebaf61fd2ba2203ef2a43af38b95b353e992ef48f00ebb190"))
        ( 225000, uint256("be148d9c5eab4a33392a6367198796784479720d06bfdd07bd547fe934eea15a"))
        ( 250000, uint256("0e4bcfe8d970979f7e30e2809ab51908d435677998cf759169407824d4f36460"))
        ( 270639, uint256("c587a36dd4f60725b9dd01d99694799bef111fc584d659f6756ab06d2a90d911"))
        ( 299742, uint256("1cc89c0c8a58046bf0222fe131c099852bd9af25a80e07922918ef5fb39d6742"))
        ( 323141, uint256("60c9f919f9b271add6ef5671e9538bad296d79f7fdc6487ba702bf2ba131d31d"))
        ( 339202, uint256("8c29048df5ae9df38a67ea9470fdd404d281a3a5c6f33080cd5bf14aa496ab03"))
        ( 350000, uint256("2bdcba23a47049e69c4fec4c425462e30f3d21d25223bde0ed36be4ea59a7075"))
	( 370005, uint256("7be5af2c5bdcb79047dcd691ef613b82d4f1c20835677daed936de37a4782e15"))
	( 371337, uint256("60323982f9c5ff1b5a954eac9dc1269352835f47c2c5222691d80f0d50dcf053"))
	( 400002, uint256("a5021d69a83f39aef10f3f24f932068d6ff322c654d20562def3fac5703ce3aa"))
        ;
    static const CCheckpointData data = {
        &mapCheckpoints,
        1412259032, // * UNIX timestamp of last checkpoint block
        12371456,   // * total number of transactions between genesis and last checkpoint
                    //   (the tx=... number in the SetBestChain debug.log lines)
        29000.0     // * estimated number of transactions per day after checkpoint
    };

    static MapCheckpoints mapCheckpointsTestnet =
        boost::assign::map_list_of
        (     0, uint256("0x"))
        ;
    static const CCheckpointData dataTestnet = {
        &mapCheckpointsTestnet,
        1369685559,
        37581,
        300
    };

    static MapCheckpoints mapCheckpointsRegtest =
        boost::assign::map_list_of
        ( 0, uint256("3d2160a3b5dc4a9d62e7e66a295f70313ac808440ef7400d6c0772171ce973a5"))
        ;
    static const CCheckpointData dataRegtest = {
        &mapCheckpointsRegtest,
        0,
        0,
        0
    };

    const CCheckpointData &Checkpoints() {
        if (Params().NetworkID() == CChainParams::TESTNET)
            return dataTestnet;
        else if (Params().NetworkID() == CChainParams::MAIN)
            return data;
        else
            return dataRegtest;
    }

    bool CheckBlock(int nHeight, const uint256& hash)
    {
        if (!fEnabled)
            return true;

        const MapCheckpoints& checkpoints = *Checkpoints().mapCheckpoints;

        MapCheckpoints::const_iterator i = checkpoints.find(nHeight);
        if (i == checkpoints.end()) return true;
        return hash == i->second;
    }

    // Guess how far we are in the verification process at the given block index
    double GuessVerificationProgress(CBlockIndex *pindex, bool fSigchecks) {
        if (pindex==NULL)
            return 0.0;

        int64_t nNow = time(NULL);

        double fSigcheckVerificationFactor = fSigchecks ? SIGCHECK_VERIFICATION_FACTOR : 1.0;
        double fWorkBefore = 0.0; // Amount of work done before pindex
        double fWorkAfter = 0.0;  // Amount of work left after pindex (estimated)
        // Work is defined as: 1.0 per transaction before the last checkpoint, and
        // fSigcheckVerificationFactor per transaction after.

        const CCheckpointData &data = Checkpoints();

        if (pindex->nChainTx <= data.nTransactionsLastCheckpoint) {
            double nCheapBefore = pindex->nChainTx;
            double nCheapAfter = data.nTransactionsLastCheckpoint - pindex->nChainTx;
            double nExpensiveAfter = (nNow - data.nTimeLastCheckpoint)/86400.0*data.fTransactionsPerDay;
            fWorkBefore = nCheapBefore;
            fWorkAfter = nCheapAfter + nExpensiveAfter*fSigcheckVerificationFactor;
        } else {
            double nCheapBefore = data.nTransactionsLastCheckpoint;
            double nExpensiveBefore = pindex->nChainTx - data.nTransactionsLastCheckpoint;
            double nExpensiveAfter = (nNow - pindex->nTime)/86400.0*data.fTransactionsPerDay;
            fWorkBefore = nCheapBefore + nExpensiveBefore*fSigcheckVerificationFactor;
            fWorkAfter = nExpensiveAfter*fSigcheckVerificationFactor;
        }

        return fWorkBefore / (fWorkBefore + fWorkAfter);
    }

    int GetTotalBlocksEstimate()
    {
        if (!fEnabled)
            return 0;

        const MapCheckpoints& checkpoints = *Checkpoints().mapCheckpoints;

        return checkpoints.rbegin()->first;
    }

    CBlockIndex* GetLastCheckpoint(const std::map<uint256, CBlockIndex*>& mapBlockIndex)
    {
        if (!fEnabled)
            return NULL;

        const MapCheckpoints& checkpoints = *Checkpoints().mapCheckpoints;

        BOOST_REVERSE_FOREACH(const MapCheckpoints::value_type& i, checkpoints)
        {
            const uint256& hash = i.second;
            std::map<uint256, CBlockIndex*>::const_iterator t = mapBlockIndex.find(hash);
            if (t != mapBlockIndex.end())
                return t->second;
        }
        return NULL;
    }
}
