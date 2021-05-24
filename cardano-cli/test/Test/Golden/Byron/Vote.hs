{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.Vote
  ( voteTests
  ) where

import           Cardano.CLI.Byron.Vote
import           Cardano.Prelude
import qualified Data.Text as Text

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

golden_byron_yes_vote :: Property
golden_byron_yes_vote = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenYesVote <- noteInputFile "test/data/golden/byron/votes/vote-yes"
  proposal <- noteInputFile "test/data/golden/byron/update-proposal"
  signingKey <- noteInputFile "test/data/golden/byron/keys/byron.skey"
  createdYesVote <- noteTempFile tempDir "byron-yes-vote"
  void $ execCardanoCLI
    [ "byron","governance","create-proposal-vote"
    , "--mainnet"
    , "--proposal-filepath", proposal
    , "--signing-key", signingKey
    , "--vote-yes"
    , "--output-filepath", createdYesVote
    ]

  eGolden <- liftIO . runExceptT $ readByronVote goldenYesVote
  golden <- case eGolden of
              Left err -> failWith Nothing . Text.unpack $ renderByronVoteError err
              Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readByronVote createdYesVote
  created <- case eCreated of
               Left err -> failWith Nothing . Text.unpack $ renderByronVoteError err
               Right prop -> return prop

  golden === created

golden_byron_no_vote :: Property
golden_byron_no_vote = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenNoVote <- noteInputFile "test/data/golden/byron/votes/vote-no"
  proposal <- noteInputFile "test/data/golden/byron/update-proposal"
  signingKey <- noteInputFile "test/data/golden/byron/keys/byron.skey"
  createdNoVote <- noteTempFile tempDir "byron-no-vote"
  void $ execCardanoCLI
    [ "byron","governance","create-proposal-vote"
    , "--mainnet"
    , "--proposal-filepath", proposal
    , "--signing-key", signingKey
    , "--vote-no"
    , "--output-filepath", createdNoVote
    ]

  eGolden <- liftIO . runExceptT $ readByronVote goldenNoVote
  golden <- case eGolden of
              Left err -> failWith Nothing . Text.unpack $ renderByronVoteError err
              Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readByronVote createdNoVote
  created <- case eCreated of
               Left err -> failWith Nothing . Text.unpack $ renderByronVoteError err
               Right prop -> return prop

  golden === created

voteTests :: IO Bool
voteTests =
  H.checkSequential
    $ H.Group "Byron Vote Goldens"
        [ ("golden_byron_no_vote", golden_byron_no_vote)
        , ("golden_byron_yes_vote", golden_byron_yes_vote)
        ]
