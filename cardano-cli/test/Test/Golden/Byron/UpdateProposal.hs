{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.UpdateProposal
  ( updateProposalTest
  ) where

import           Cardano.Prelude
import qualified Data.Text as Text

import           Cardano.CLI.Byron.UpdateProposal

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

golden_byron_update_proposal :: Property
golden_byron_update_proposal = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenUpdateProposal <- noteInputFile "test/data/golden/byron/update-proposal"
  signingKey <- noteInputFile "test/data/golden/byron/keys/byron.skey"
  createdUpdateProposal <- noteTempFile tempDir "byron-update-proposal"
  void $ execCardanoCLI
    [ "byron","governance","create-update-proposal"
    , "--mainnet"
    , "--signing-key", signingKey
    , "--protocol-version-major", "1"
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    , "--filepath", createdUpdateProposal
    ]

  eGolden <- liftIO . runExceptT $ readByronUpdateProposal goldenUpdateProposal
  golden <- case eGolden of
              Left err -> failWith Nothing . Text.unpack $ renderByronUpdateProposalError err
              Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readByronUpdateProposal createdUpdateProposal
  created <- case eCreated of
               Left err -> failWith Nothing . Text.unpack $ renderByronUpdateProposalError err
               Right prop -> return prop

  golden === created

updateProposalTest :: IO Bool
updateProposalTest =
  H.checkSequential
    $ H.Group "Byron Update Proposal Golden"
        [ ("golden_byron_update_proposal", golden_byron_update_proposal)
        ]
