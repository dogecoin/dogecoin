{-# LANGUAGE OverloadedStrings #-}
module Cardano.Api.TxSubmit.ErrorRender
  ( renderApplyMempoolPayloadErr
  ) where

-- This file contains error renders. They should have been defined at a lower level, with the error
-- type definitions, but for some reason have not been.
-- They will be defined here for now and then moved where they are supposed to be once they
-- are working.

import           Cardano.Chain.Byron.API (ApplyMempoolPayloadErr (..))
import           Cardano.Chain.UTxO.UTxO (UTxOError (..))
import           Cardano.Chain.UTxO.Validation (TxValidationError (..), UTxOValidationError (..))

import           Cardano.Prelude hiding ((%))

import qualified Data.Text as Text

import           Formatting (build, sformat, stext, (%))

renderApplyMempoolPayloadErr :: ApplyMempoolPayloadErr -> Text
renderApplyMempoolPayloadErr err =
    case err of
      MempoolTxErr ve -> renderValidationError ve
      MempoolDlgErr {} -> "Delegation error"
      MempoolUpdateProposalErr {} -> "Update proposal error"
      MempoolUpdateVoteErr {} -> "Update vote error"


renderValidationError :: UTxOValidationError -> Text
renderValidationError ve =
  case ve of
    UTxOValidationTxValidationError tve -> renderTxValidationError tve
    UTxOValidationUTxOError ue -> renderUTxOError ue


renderTxValidationError :: TxValidationError -> Text
renderTxValidationError tve =
  "Tx Validation: " <>
    case tve of
      TxValidationLovelaceError txt e ->
        sformat ("Lovelace error "% stext %": "% build) txt e
      TxValidationFeeTooSmall tx expected actual ->
        sformat ("Tx "% build %" fee "% build %"too low, expected "% build) tx actual expected
      TxValidationWitnessWrongSignature wit pmid sig ->
        sformat ("Bad witness "% build %" for signature "% stext %" protocol magic id "% stext) wit (textShow sig) (textShow pmid)
      TxValidationWitnessWrongKey wit addr ->
        sformat ("Bad witness "% build %" for address "% build) wit addr
      TxValidationMissingInput tx ->
        sformat ("Validation cannot find input tx "% build) tx
      -- Fields are <expected> <actual>
      TxValidationNetworkMagicMismatch expected actual ->
        mconcat [ "Bad network magic  ", textShow actual, ", expected ", textShow expected ]
      TxValidationTxTooLarge expected actual ->
        mconcat [ "Tx is ", textShow actual, " bytes, but expected < ", textShow expected, " bytes" ]
      TxValidationUnknownAddressAttributes ->
        "Unknown address attributes"
      TxValidationUnknownAttributes ->
        "Unknown attributes"

renderUTxOError :: UTxOError -> Text
renderUTxOError ue =
  "UTxOError: " <>
    case ue of
      UTxOMissingInput tx -> sformat ("Lookup of tx "% build %" failed") tx
      UTxOOverlappingUnion -> "Union or two overlapping UTxO sets"

textShow :: Show a => a -> Text
textShow = Text.pack . show
