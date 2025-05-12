// Copyright (c) 2009-2014 The Bitcoin Core developers
// Copyright (c) 2011 Vince Durham
// Copyright (c) 2014-2016 Daniel Kraft
// Copyright (c) 2021-2025 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "rpc/mining.h"

#include "base58.h"
#include "chain.h"
#include "chainparams.h"
#include "consensus/consensus.h"
#include "consensus/params.h"
#include "core_io.h"
#include "init.h"
#include "miner.h"
#include "net.h"
#include "rpc/auxcache.h"
#include "rpc/server.h"
#include "util.h"
#include "utilstrencodings.h"
#include "validation.h"
#include "validationinterface.h"

#include <stdint.h>
#include <memory>
#include <vector>

#include <univalue.h>

bool fUseNamecoinApi;

static CCriticalSection cs_auxpowrpc;
static CAuxBlockCache auxBlockCache;
static std::vector<std::unique_ptr<CBlockTemplate>> vNewBlockTemplate;

void AuxMiningCheck()
{
    if(!g_connman)
        throw JSONRPCError(RPC_CLIENT_P2P_DISABLED, "Error: Peer-to-peer functionality missing or disabled");

    if (g_connman->GetNodeCount(CConnman::CONNECTIONS_ALL) == 0 && !Params().MineBlocksOnDemand())
        throw JSONRPCError(RPC_CLIENT_NOT_CONNECTED, "Dogecoin is not connected!");

    if (IsInitialBlockDownload() && !Params().MineBlocksOnDemand())
        throw JSONRPCError(RPC_CLIENT_IN_INITIAL_DOWNLOAD,
                           "Dogecoin is downloading blocks...");

    /* This should never fail, since the chain is already
       past the point of merge-mining start.  Check nevertheless.  */
    {
        LOCK(cs_main);
        if (Params().GetConsensus(chainActive.Height() + 1).fAllowLegacyBlocks)
            throw std::runtime_error("getauxblock method is not yet available");
    }
}

static UniValue AuxMiningCreateBlock(const CScript& scriptPubKey)
{
    AuxMiningCheck();
    LOCK(cs_auxpowrpc);

    static unsigned int nTransactionsUpdatedLast;
    static const CBlockIndex* pindexPrev = nullptr;
    static uint64_t nStart;
    static unsigned nExtraNonce = 0;

    // Dogecoin: Never mine witness tx
    const bool fMineWitnessTx = false;

    /* Search for cached blocks with given scriptPubKey and assign it to pBlock
     * if we find a match. This allows for creating multiple aux templates with
     * a single dogecoind instance, for example when a pool runs multiple sub-
     * pools with different payout strategies.
     */
    std::shared_ptr<CBlock> pblock;
    CScriptID scriptID (scriptPubKey);
    auxBlockCache.Get(scriptID, pblock);
    {
        LOCK(cs_main);

        // Update block
        if (!pblock || pindexPrev != chainActive.Tip()
            || (mempool.GetTransactionsUpdated() != nTransactionsUpdatedLast
                && GetTime() - nStart > 60))
        {
            if (pindexPrev != chainActive.Tip())
            {
                // Clear caches since they're obsolete now.
                auxBlockCache.Reset();
                vNewBlockTemplate.clear();
                pblock.reset();
            }

            // Create new block with nonce = 0 and extraNonce = 1
            std::unique_ptr<CBlockTemplate> newBlock
                = BlockAssembler(Params()).CreateNewBlock(scriptPubKey, fMineWitnessTx);
            if (!newBlock)
                throw JSONRPCError(RPC_OUT_OF_MEMORY, "out of memory");

            // Update state only when CreateNewBlock succeeded
            nTransactionsUpdatedLast = mempool.GetTransactionsUpdated();
            pindexPrev = chainActive.Tip();
            nStart = GetTime();

            // Finalise it by setting the version and building the merkle root
            IncrementExtraNonce(&newBlock->block, pindexPrev, nExtraNonce);
            newBlock->block.SetAuxpowFlag(true);

            // Save
            pblock = std::make_shared<CBlock>(newBlock->block);
            auxBlockCache.Add(scriptID, pblock);
            vNewBlockTemplate.push_back(std::move(newBlock));
        }
    }

    // At this point, pblock is always initialised:  If we make it here
    // without creating a new block above, it means that, in particular,
    // pindexPrev == chainActive.Tip().  But for that to happen, we must
    // already have created a pblock in a previous call, as pindexPrev is
    // initialised only when pblock is.
    assert(pblock);

    arith_uint256 target;
    bool fNegative, fOverflow;
    target.SetCompact(pblock->nBits, &fNegative, &fOverflow);
    if (fNegative || fOverflow || target == 0)
        throw std::runtime_error("invalid difficulty bits in block");

    UniValue result(UniValue::VOBJ);
    result.pushKV("hash", pblock->GetHash().GetHex());
    result.pushKV("chainid", pblock->GetChainId());
    result.pushKV("previousblockhash", pblock->hashPrevBlock.GetHex());
    result.pushKV("coinbasevalue", (int64_t)pblock->vtx[0]->vout[0].nValue);
    result.pushKV("bits", strprintf("%08x", pblock->nBits));
    result.pushKV("height", static_cast<int64_t> (pindexPrev->nHeight + 1));
    result.pushKV(fUseNamecoinApi ? "_target" : "target", HexStr(BEGIN(target), END(target)));

    return result;
}

static UniValue AuxMiningSubmitBlock(const uint256 hash, const CAuxPow auxpow)
{
    AuxMiningCheck();
    LOCK(cs_auxpowrpc);

    std::shared_ptr<CBlock> pblock;
    if (!auxBlockCache.Get(hash, pblock)) {
        throw JSONRPCError(RPC_INVALID_PARAMETER, "block hash unknown");
    }
    CBlock& block = *pblock;
    block.SetAuxpow(new CAuxPow(auxpow));
    assert(block.GetHash() == hash);

    submitblock_StateCatcher sc(block.GetHash());
    RegisterValidationInterface(&sc);
    std::shared_ptr<const CBlock> shared_block = std::make_shared<const CBlock>(block);
    ProcessNewBlock(Params(), shared_block, true, nullptr);
    UnregisterValidationInterface(&sc);

    return BIP22ValidationResult(sc.state);
}

UniValue createauxblock(const JSONRPCRequest& request)
{
    if (request.fHelp || request.params.size() != 1)
        throw std::runtime_error(
            "createauxblock <address>\n"
            "\ncreate a new block and return information required to merge-mine it.\n"
            "\nArguments:\n"
            "1. address      (string, required) specify coinbase transaction payout address\n"
            "\nResult:\n"
            "{\n"
            "  \"hash\"               (string) hash of the created block\n"
            "  \"chainid\"            (numeric) chain ID for this block\n"
            "  \"previousblockhash\"  (string) hash of the previous block\n"
            "  \"coinbasevalue\"      (numeric) value of the block's coinbase\n"
            "  \"bits\"               (string) compressed target of the block\n"
            "  \"height\"             (numeric) height of the block\n"
            + (std::string) (
              fUseNamecoinApi
              ? "  \"_target\"            (string) target in reversed byte order\n"
              : "  \"target\"             (string) target in reversed byte order\n"
            )
            + "}\n"
            "\nExamples:\n"
            + HelpExampleCli("createauxblock", "\"address\"")
            + HelpExampleRpc("createauxblock", "\"address\"")
            );

    // Check coinbase payout address
    CBitcoinAddress coinbaseAddress(request.params[0].get_str());

    if (!coinbaseAddress.IsValid())
        throw JSONRPCError(RPC_INVALID_PARAMETER,"Invalid coinbase payout address");

    const CScript scriptPubKey = GetScriptForDestination(coinbaseAddress.Get());
    return AuxMiningCreateBlock(scriptPubKey);
}

UniValue submitauxblock(const JSONRPCRequest& request)
{
    if (request.fHelp || request.params.size() != 2)
        throw std::runtime_error(
            "submitauxblock <hash> <auxpow>\n"
            "\nsubmit a solved auxpow for a previously block created by 'createauxblock'.\n"
            "\nArguments:\n"
            "1. hash      (string, required) hash of the block to submit\n"
            "2. auxpow    (string, required) serialised auxpow found\n"
            "\nResult:\n"
            "xxxxx        (boolean) whether the submitted block was correct\n"
            "\nExamples:\n"
            + HelpExampleCli("submitauxblock", "\"hash\" \"serialised auxpow\"")
            + HelpExampleRpc("submitauxblock", "\"hash\" \"serialised auxpow\"")
            );

    const uint256 hash = ParseHashV(request.params[0], "hash");
    CAuxPow auxpow;
    if (!DecodeAuxPow(auxpow, request.params[1].get_str())) {
        throw JSONRPCError(RPC_DESERIALIZATION_ERROR, "AuxPow decode failed");
    }

    UniValue response = AuxMiningSubmitBlock(hash, auxpow);

    return response.isNull();
}

UniValue getauxblock(const JSONRPCRequest& request)
{
  if (request.fHelp
        || (request.params.size() != 0 && request.params.size() != 2))
      throw std::runtime_error(
          "getauxblock (hash auxpow)\n"
          "\nCreate or submit a merge-mined block.\n"
          "\nWithout arguments, create a new block and return information\n"
          "required to merge-mine it.  With arguments, submit a solved\n"
          "auxpow for a previously returned block.\n"
          "\nArguments:\n"
          "1. hash      (string, optional) hash of the block to submit\n"
          "2. auxpow    (string, optional) serialised auxpow found\n"
          "\nResult (without arguments):\n"
          "{\n"
          "  \"hash\"               (string) hash of the created block\n"
          "  \"chainid\"            (numeric) chain ID for this block\n"
          "  \"previousblockhash\"  (string) hash of the previous block\n"
          "  \"coinbasevalue\"      (numeric) value of the block's coinbase\n"
          "  \"bits\"               (string) compressed target of the block\n"
          "  \"height\"             (numeric) height of the block\n"
          + (std::string) (
            fUseNamecoinApi
            ? "  \"_target\"            (string) target in reversed byte order\n"
            : "  \"target\"             (string) target in reversed byte order\n"
          )
          + "}\n"
          "\nResult (with arguments):\n"
          "xxxxx        (boolean) whether the submitted block was correct\n"
          "\nExamples:\n"
          + HelpExampleCli("getauxblock", "")
          + HelpExampleCli("getauxblock", "\"hash\" \"serialised auxpow\"")
          + HelpExampleRpc("getauxblock", "")
          );

  AuxMiningCheck();

  std::shared_ptr<CReserveScript> coinbaseScript;
  GetMainSignals().ScriptForMining(coinbaseScript);

  // If the keypool is exhausted, no script is returned at all.  Catch this.
  if (!coinbaseScript)
      throw JSONRPCError(RPC_WALLET_KEYPOOL_RAN_OUT, "Error: Keypool ran out, please call keypoolrefill first");

  //throw an error if no script was provided
  if (!coinbaseScript->reserveScript.size())
      throw JSONRPCError(RPC_INTERNAL_ERROR, "No coinbase script available (mining requires a wallet)");

  /* Create a new block?  */
  if (request.params.size() == 0)
  {
      return AuxMiningCreateBlock(coinbaseScript->reserveScript);
  }

  /* Submit a block instead. */
  assert(request.params.size() == 2);

  const uint256 hash = ParseHashV(request.params[0], "hash");
  CAuxPow auxpow;
  if (!DecodeAuxPow(auxpow, request.params[1].get_str())) {
      throw JSONRPCError(RPC_DESERIALIZATION_ERROR, "AuxPow decode failed");
  }

  UniValue response = AuxMiningSubmitBlock(hash, auxpow);

  if (response.isNull()) {
      coinbaseScript->KeepScript();
  }

  return response.isNull();
}

static const CRPCCommand commands[] =
{ //  category              name                      actor (function)         okSafeMode
  //  --------------------- ------------------------  -----------------------  ----------
    { "mining",             "getauxblock",            &getauxblock,            true,  {"hash", "auxpow"} },
    { "mining",             "createauxblock",         &createauxblock,         true,  {"address"} },
    { "mining",             "submitauxblock",         &submitauxblock,         true,  {"hash", "auxpow"} },
};

void RegisterAuxPoWRPCCommands(CRPCTable &t)
{
    for (unsigned int vcidx = 0; vcidx < ARRAYLEN(commands); vcidx++)
        t.appendCommand(commands[vcidx].name, &commands[vcidx]);
}
