// Copyright (c) 2009-2010 Satoshi Nakamoto
// Distributed under the MIT/X11 software license, see the accompanying
// file license.txt or http://www.opensource.org/licenses/mit-license.php.
#ifndef BITCOIN_AUXPOW_H
#define BITCOIN_AUXPOW_H

#include "main.h"

class CAuxPow : public CMerkleTx
{
public:
    CAuxPow(const CTransaction& txIn) : CMerkleTx(txIn)
    {
    }

    CAuxPow() :CMerkleTx()
    {
    }

    // Merkle branch with root vchAux
    // root must be present inside the coinbase
    std::vector<uint256> vChainMerkleBranch;
    // Index of chain in chains merkle tree
    unsigned int nChainIndex;
    CBlockHeader parentBlockHeader;

    IMPLEMENT_SERIALIZE
    (
        nSerSize += SerReadWrite(s, *(CMerkleTx*)this, nType, nVersion, ser_action);
        nVersion = this->nVersion;
        READWRITE(vChainMerkleBranch);
        READWRITE(nChainIndex);

        // Always serialize the saved parent block as header so that the size of CAuxPow
        // is consistent.
        nSerSize += SerReadWrite(s, parentBlockHeader, nType, nVersion, ser_action);
    )

    bool Check(uint256 hashAuxBlock, int nChainID);

    uint256 GetParentBlockHash()
    {
        return parentBlockHeader.GetPoWHash();
    }
};

template <typename Stream>
int ReadWriteAuxPow(Stream& s, const boost::shared_ptr<CAuxPow>& auxpow, int nType, int nVersion, CSerActionGetSerializeSize ser_action)
{
    if (nVersion & BLOCK_VERSION_AUXPOW)
    {
        return ::GetSerializeSize(*auxpow, nType, nVersion);
    }
    return 0;
}

template <typename Stream>
int ReadWriteAuxPow(Stream& s, const boost::shared_ptr<CAuxPow>& auxpow, int nType, int nVersion, CSerActionSerialize ser_action)
{
    if (nVersion & BLOCK_VERSION_AUXPOW)
    {
        return SerReadWrite(s, *auxpow, nType, nVersion, ser_action);
    }
    return 0;
}

template <typename Stream>
int ReadWriteAuxPow(Stream& s, boost::shared_ptr<CAuxPow>& auxpow, int nType, int nVersion, CSerActionUnserialize ser_action)
{
    if (nVersion & BLOCK_VERSION_AUXPOW)
    {
        auxpow.reset(new CAuxPow());
        return SerReadWrite(s, *auxpow, nType, nVersion, ser_action);
    }
    else
    {
        auxpow.reset();
        return 0;
    }
}

extern void RemoveMergedMiningHeader(std::vector<unsigned char>& vchAux);
extern CScript MakeCoinbaseWithAux(unsigned int nBits, unsigned int nExtraNonce, std::vector<unsigned char>& vchAux);
#endif
