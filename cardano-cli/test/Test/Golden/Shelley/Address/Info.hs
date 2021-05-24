{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.Address.Info
  ( golden_shelleyAddressInfo
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Data.List as L
import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressInfo :: Property
golden_shelleyAddressInfo = propertyOnce $ do
  -- Disable as per commit: e69984d797fc3bdd5d71bdd99a0328110d71f6ad
  when False $ do
    let byronBase58 = "DdzFFzCqrhsg9F1joqXWJdGKwn6MaNavCqPsrZcjADRjA4ifEtrBmREJZyCojtuexKjMKNFr6CoU7Gx6PPR7pq14JxvPZuuk2xVkzn8p"

    infoText1 <- execCardanoCLI
      [ "address","info"
      , "--address", byronBase58
      ]

    H.assert $ "Encoding: Base58" `L.isInfixOf` infoText1
    H.assert $ "Era: Byron" `L.isInfixOf` infoText1

    let byronHex = "82d818584283581c120e97e4ca7b831373c1060853d4896314e17d567a5723879b9a20eaa101581e581c135a115dd5dba68c28fb7e9409729ffc0503219ff7f9c08e84d13319001a28d0b871"

    infoText2 <- execCardanoCLI
      [ "address","info"
      , "--address", byronHex
      ]

    H.assert $ "Encoding: Hex" `L.isInfixOf` infoText2
    H.assert $ "Era: Byron" `L.isInfixOf` infoText2

    let shelleyHex = "82065820d8b4a892f2f6f1820d350c207d17d4cd7e7a1f7e0a83059e2d698a65ab8f96ed"

    infoText3 <- execCardanoCLI
      [ "address","info"
      , "--address", shelleyHex
      ]

    H.assert $ "Encoding: Hex" `L.isInfixOf` infoText3
    H.assert $ "Era: Shelley" `L.isInfixOf` infoText3
