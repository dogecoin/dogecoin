{-# LANGUAGE TypeFamilies #-}

-- | Special Byron values that we can submit to a node to propose an update proposal
-- or to vote on an update proposal. These are not transactions.
--
module Cardano.Api.SpecialByron
  ( ByronUpdateProposal(..),
    ByronProtocolParametersUpdate(..),
    AsType(AsByronUpdateProposal, AsByronVote),
    makeProtocolParametersUpdate,
    toByronLedgerUpdateProposal,
    ByronVote(..),
    makeByronUpdateProposal,
    makeByronVote,
    toByronLedgertoByronVote,
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import           Data.Word
import           Numeric.Natural

import           Cardano.Api.HasTypeProxy
import           Cardano.Api.KeysByron
import           Cardano.Api.NetworkId (NetworkId, toByronProtocolMagicId)
import           Cardano.Api.SerialiseRaw

import qualified Cardano.Binary as Binary
import           Cardano.Chain.Common (LovelacePortion, TxFeePolicy)
import           Cardano.Chain.Slotting
import           Cardano.Chain.Update (AProposal (aBody, annotation), InstallerHash,
                     ProposalBody (ProposalBody), ProtocolParametersUpdate (..), ProtocolVersion,
                     SoftforkRule, SoftwareVersion, SystemTag, UpId, mkVote, recoverUpId,
                     recoverVoteId, signProposal)
import qualified Cardano.Chain.Update.Vote as ByronVote
import           Cardano.Crypto (SafeSigner, noPassSafeSigner)

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool

{- HLINT ignore "Use void" -}

-- | Byron era update proposal

newtype ByronUpdateProposal =
    ByronUpdateProposal { unByronUpdateProposal :: AProposal ByteString}
  deriving (Eq, Show)

instance HasTypeProxy ByronUpdateProposal where
  data AsType ByronUpdateProposal = AsByronUpdateProposal
  proxyToAsType _ = AsByronUpdateProposal

instance SerialiseAsRawBytes ByronUpdateProposal where
  serialiseToRawBytes (ByronUpdateProposal proposal) = annotation proposal
  deserialiseFromRawBytes AsByronUpdateProposal bs =
    let lBs = LB.fromStrict bs
    in case Binary.decodeFull lBs of
        Left _deserFail -> Nothing
        Right proposal -> Just (ByronUpdateProposal proposal')
          where
            proposal' :: AProposal ByteString
            proposal' = Binary.annotationBytes lBs proposal

makeByronUpdateProposal
  :: NetworkId
  -> ProtocolVersion
  -> SoftwareVersion
  -> SystemTag
  -> InstallerHash
  -> SomeByronSigningKey
  -> ByronProtocolParametersUpdate
  -> ByronUpdateProposal
makeByronUpdateProposal nId pVer sVer sysTag insHash
                          bWit paramsToUpdate =
  let nonAnnotatedProposal :: AProposal ()
      nonAnnotatedProposal = signProposal (toByronProtocolMagicId nId) proposalBody noPassSigningKey
      annotatedPropBody :: Binary.Annotated ProposalBody ByteString
      annotatedPropBody = Binary.reAnnotate $ aBody nonAnnotatedProposal
  in ByronUpdateProposal
       $ nonAnnotatedProposal { aBody = annotatedPropBody
                              , annotation = Binary.serialize' nonAnnotatedProposal
                              }
 where
   proposalBody :: ProposalBody
   proposalBody = ProposalBody pVer protocolParamsUpdate sVer metaData

   metaData :: M.Map SystemTag InstallerHash
   metaData = M.singleton sysTag insHash

   noPassSigningKey :: SafeSigner
   noPassSigningKey = noPassSafeSigner $ toByronSigningKey bWit

   protocolParamsUpdate :: ProtocolParametersUpdate
   protocolParamsUpdate = makeProtocolParametersUpdate paramsToUpdate

data ByronProtocolParametersUpdate =
  ByronProtocolParametersUpdate
    { bPpuScriptVersion     :: !(Maybe Word16)
      -- ^ Redundant. This was meant to be the version of the
      -- Plutus smart contract language, however, there are no
      -- smart contracts nor scripts in the Byron era.
    , bPpuSlotDuration      :: !(Maybe Natural)
      -- ^ Slot duration in milliseconds.
    , bPpuMaxBlockSize      :: !(Maybe Natural)
      -- ^ Maximum block size in bytes.
    , bPpuMaxHeaderSize     :: !(Maybe Natural)
      -- ^ Maximum block header size in bytes.
    , bPpuMaxTxSize         :: !(Maybe Natural)
      -- ^ Maxiumum transaction size in bytes.
    , bPpuMaxProposalSize   :: !(Maybe Natural)
      -- ^ Maximum update proposal size in bytes.
    , bPpuMpcThd            :: !(Maybe LovelacePortion)
    , bPpuHeavyDelThd       :: !(Maybe LovelacePortion)
      -- ^ Heavyweight delegation threshold. The delegate (i.e stakeholder)
      -- must possess no less than this threshold of stake in order to participate
      -- in heavyweight delegation.
    , bPpuUpdateVoteThd     :: !(Maybe LovelacePortion)
    , bPpuUpdateProposalThd :: !(Maybe LovelacePortion)
    , bPpuUpdateProposalTTL :: !(Maybe SlotNumber)
    , bPpuSoftforkRule      :: !(Maybe SoftforkRule)
      -- ^ Values defining the softfork resolution rule. When the stake belonging
      -- to block issuers, issuing a given block version, is greater than the
      -- current softfork resolution threshold, this block version is adopted.
    , bPpuTxFeePolicy       :: !(Maybe TxFeePolicy)
      -- ^ Transaction fee policy represents a formula to compute the minimal allowed
      -- Fee for a transaction. Transactions with lesser fees won't be accepted.
    , bPpuUnlockStakeEpoch  :: !(Maybe EpochNumber)
      -- ^ This has been re-purposed for unlocking the OuroborosBFT logic in the software.
      -- Relevant: [CDEC-610](https://iohk.myjetbrains.com/youtrack/issue/CDEC-610)
    } deriving Show

makeProtocolParametersUpdate
  :: ByronProtocolParametersUpdate
  -> ProtocolParametersUpdate
makeProtocolParametersUpdate apiPpu =
  ProtocolParametersUpdate
    { ppuScriptVersion = bPpuScriptVersion apiPpu
    , ppuSlotDuration = bPpuSlotDuration apiPpu
    , ppuMaxBlockSize = bPpuMaxBlockSize apiPpu
    , ppuMaxHeaderSize = bPpuMaxHeaderSize apiPpu
    , ppuMaxTxSize = bPpuMaxTxSize apiPpu
    , ppuMaxProposalSize = bPpuMaxProposalSize apiPpu
    , ppuMpcThd = bPpuMpcThd apiPpu
    , ppuHeavyDelThd = bPpuHeavyDelThd apiPpu
    , ppuUpdateVoteThd = bPpuUpdateVoteThd apiPpu
    , ppuUpdateProposalThd = bPpuUpdateProposalThd apiPpu
    , ppuUpdateProposalTTL = bPpuUpdateProposalTTL apiPpu
    , ppuSoftforkRule = bPpuSoftforkRule apiPpu
    , ppuTxFeePolicy = bPpuTxFeePolicy apiPpu
    , ppuUnlockStakeEpoch = bPpuUnlockStakeEpoch apiPpu
    }

toByronLedgerUpdateProposal :: ByronUpdateProposal -> Mempool.GenTx ByronBlock
toByronLedgerUpdateProposal (ByronUpdateProposal proposal) =
  Mempool.ByronUpdateProposal (recoverUpId proposal) proposal

-- | Byron era votes

newtype ByronVote = ByronVote { unByronVote :: ByronVote.AVote ByteString }
  deriving (Eq, Show)

instance HasTypeProxy ByronVote where
  data AsType ByronVote = AsByronVote
  proxyToAsType _ = AsByronVote

instance SerialiseAsRawBytes ByronVote where
  serialiseToRawBytes (ByronVote vote) = Binary.serialize' $ fmap (const ()) vote
  deserialiseFromRawBytes AsByronVote bs =
    let lBs = LB.fromStrict bs
    in case Binary.decodeFull lBs of
         Left _deserFail -> Nothing
         Right vote -> Just . ByronVote $ annotateVote vote lBs
   where
    annotateVote :: ByronVote.AVote Binary.ByteSpan -> LB.ByteString -> ByronVote.AVote ByteString
    annotateVote vote bs' = Binary.annotationBytes bs' vote


makeByronVote
  :: NetworkId
  -> SomeByronSigningKey
  -> ByronUpdateProposal
  -> Bool
  -> ByronVote
makeByronVote nId sKey (ByronUpdateProposal proposal) yesOrNo =
  let signingKey = toByronSigningKey sKey
      nonAnnotatedVote :: ByronVote.AVote ()
      nonAnnotatedVote = mkVote (toByronProtocolMagicId nId) signingKey (recoverUpId proposal) yesOrNo
      annotatedProposalId :: Binary.Annotated UpId ByteString
      annotatedProposalId = Binary.reAnnotate $ ByronVote.aProposalId nonAnnotatedVote
  in ByronVote
       $ nonAnnotatedVote { ByronVote.aProposalId = annotatedProposalId
                          , ByronVote.annotation = Binary.annotation annotatedProposalId
                          }

toByronLedgertoByronVote :: ByronVote -> Mempool.GenTx ByronBlock
toByronLedgertoByronVote (ByronVote vote) = Mempool.ByronUpdateVote (recoverVoteId vote) vote
