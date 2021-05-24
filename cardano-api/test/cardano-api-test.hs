
import           Cardano.Crypto.Libsodium (sodiumInit)
import           Cardano.Prelude

import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.Api.Crypto
import qualified Test.Cardano.Api.Ledger
import qualified Test.Cardano.Api.Metadata
import qualified Test.Cardano.Api.Typed.Bech32
import qualified Test.Cardano.Api.Typed.CBOR
import qualified Test.Cardano.Api.Typed.Envelope
import qualified Test.Cardano.Api.Typed.JSON
import qualified Test.Cardano.Api.Typed.Script
import qualified Test.Cardano.Api.Typed.RawBytes
import qualified Test.Cardano.Api.Typed.Value

main :: IO ()
main = do
  -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
  sodiumInit
  defaultMain tests

tests :: TestTree
tests =
  testGroup "Cardano.Api"
    [ Test.Cardano.Api.Typed.Value.tests
    , Test.Cardano.Api.Crypto.tests
    , Test.Cardano.Api.Ledger.tests
    , Test.Cardano.Api.Metadata.tests
    , Test.Cardano.Api.Typed.Bech32.tests
    , Test.Cardano.Api.Typed.CBOR.tests
    , Test.Cardano.Api.Typed.Envelope.tests
    , Test.Cardano.Api.Typed.JSON.tests
    , Test.Cardano.Api.Typed.Script.tests
    , Test.Cardano.Api.Typed.RawBytes.tests
    ]
