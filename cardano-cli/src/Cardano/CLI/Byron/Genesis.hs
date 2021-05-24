{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.CLI.Byron.Genesis
  ( ByronGenesisError(..)
  , GenesisParameters(..)
  , NewDirectory(..)
  , dumpGenesis
  , mkGenesis
  , readGenesis
  , renderByronGenesisError
  )
where

import           Cardano.Prelude hiding (option, show, trace)
import           Prelude (String)

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, right)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Time (UTCTime)
import           Formatting.Buildable
import           Text.Printf (printf)

import           System.Directory (createDirectory, doesPathExist)
import           System.FilePath ((</>))
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions)
#endif
import           Cardano.Api (Key (..), NetworkId)
import           Cardano.Api.Byron (ByronKey, SerialiseAsRawBytes (..), SigningKey (..),
                     toByronRequiresNetworkMagic)

import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Delegation hiding (Map, epoch)
import           Cardano.Chain.Genesis (GeneratedSecrets (..))
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO as UTxO

import qualified Cardano.Crypto as Crypto

import           Cardano.CLI.Byron.Delegation
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Types (GenesisFile (..))

data ByronGenesisError
  = ByronDelegationCertSerializationError !ByronDelegationError
  | ByronDelegationKeySerializationError ByronDelegationError
  | GenesisGenerationError !Genesis.GenesisDataGenerationError
  | GenesisOutputDirAlreadyExists FilePath
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenesisSpecError !Text
  | MakeGenesisDelegationError !Genesis.GenesisDelegationError
  | NoGenesisDelegationForKey !Text
  | ProtocolParametersParseFailed !FilePath !Text
  | PoorKeyFailure !ByronKeyFailure

  deriving Show

renderByronGenesisError :: ByronGenesisError -> Text
renderByronGenesisError err =
  case err of
    ProtocolParametersParseFailed pParamFp parseError ->
      "Protocol parameters parse failed at: " <> textShow pParamFp <> " Error: " <> parseError
    ByronDelegationCertSerializationError bDelegSerErr ->
      "Error while serializing the delegation certificate: " <> textShow bDelegSerErr
    ByronDelegationKeySerializationError bKeySerErr ->
      "Error while serializing the delegation key: " <> textShow bKeySerErr
    PoorKeyFailure bKeyFailure ->
      "Error creating poor keys: " <> textShow bKeyFailure
    MakeGenesisDelegationError genDelegError ->
      "Error creating genesis delegation: " <> textShow genDelegError
    GenesisGenerationError genDataGenError ->
      "Error generating genesis: " <> textShow genDataGenError
    GenesisOutputDirAlreadyExists genOutDir ->
      "Genesis output directory already exists: " <> textShow genOutDir
    GenesisReadError genFp genDataError ->
      "Error while reading genesis file at: " <> textShow genFp <> " Error: " <> textShow genDataError
    GenesisSpecError genSpecError ->
      "Error while creating genesis spec" <> textShow genSpecError
    NoGenesisDelegationForKey verKey ->
      "Error while creating genesis, no delegation certificate for this verification key:" <> textShow verKey

newtype NewDirectory =
  NewDirectory FilePath
  deriving (Eq, Ord, Show, IsString)

-- | Parameters required for generation of new genesis.
data GenesisParameters = GenesisParameters
  { gpStartTime :: !UTCTime
  , gpProtocolParamsFile :: !FilePath
  , gpK :: !Common.BlockCount
  , gpProtocolMagic :: !Crypto.ProtocolMagic
  , gpTestnetBalance :: !Genesis.TestnetBalanceOptions
  , gpFakeAvvmOptions :: !Genesis.FakeAvvmOptions
  , gpAvvmBalanceFactor :: !Common.LovelacePortion
  , gpSeed :: !(Maybe Integer)
  } deriving Show


mkGenesisSpec :: GenesisParameters -> ExceptT ByronGenesisError IO Genesis.GenesisSpec
mkGenesisSpec gp = do
  protoParamsRaw <- lift . LB.readFile $ gpProtocolParamsFile gp

  protocolParameters <- withExceptT
    (ProtocolParametersParseFailed (gpProtocolParamsFile gp)) $
    ExceptT . pure $ canonicalDecodePretty protoParamsRaw

  -- We're relying on the generator to fake AVVM and delegation.
  genesisDelegation <- withExceptT MakeGenesisDelegationError $
    Genesis.mkGenesisDelegation []

  withExceptT GenesisSpecError $
    ExceptT . pure $ Genesis.mkGenesisSpec
      (Genesis.GenesisAvvmBalances mempty)
      genesisDelegation
      protocolParameters
      (gpK gp)
      (gpProtocolMagic gp)
      (mkGenesisInitialiser True)

  where
    mkGenesisInitialiser :: Bool -> Genesis.GenesisInitializer
    mkGenesisInitialiser useHeavyDlg =
      Genesis.GenesisInitializer
      (gpTestnetBalance gp)
      (gpFakeAvvmOptions gp)
      (Common.lovelacePortionToRational (gpAvvmBalanceFactor gp))
      useHeavyDlg

-- | Generate a genesis, for given blockchain start time, protocol parameters,
-- security parameter, protocol magic, testnet balance options, fake AVVM options,
-- AVVM balance factor and seed.  Throw an error in the following cases: if the
-- protocol parameters file can't be read or fails parse, if genesis delegation
-- couldn't be generated, if the parameter-derived genesis specification is wrong,
-- or if the genesis fails generation.
mkGenesis
  :: GenesisParameters
  -> ExceptT ByronGenesisError IO (Genesis.GenesisData, Genesis.GeneratedSecrets)
mkGenesis gp = do
  genesisSpec <- mkGenesisSpec gp

  withExceptT GenesisGenerationError $
    Genesis.generateGenesisData (gpStartTime gp) genesisSpec

-- | Read genesis from a file.
readGenesis :: GenesisFile
            -> NetworkId
            -> ExceptT ByronGenesisError IO Genesis.Config
readGenesis (GenesisFile file) nw =
  firstExceptT (GenesisReadError file) $ do
    (genesisData, genesisHash) <- Genesis.readGenesisData file
    return Genesis.Config {
      Genesis.configGenesisData       = genesisData,
      Genesis.configGenesisHash       = genesisHash,
      Genesis.configReqNetMagic       = toByronRequiresNetworkMagic nw,
      Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
    }

-- | Write out genesis into a directory that must not yet exist.  An error is
-- thrown if the directory already exists, or the genesis has delegate keys that
-- are not delegated to.
dumpGenesis
  :: NewDirectory
  -> Genesis.GenesisData
  -> Genesis.GeneratedSecrets
  -> ExceptT ByronGenesisError IO ()
dumpGenesis (NewDirectory outDir) genesisData gs = do
  exists <- liftIO $ doesPathExist outDir
  if exists
  then left $ GenesisOutputDirAlreadyExists outDir
  else liftIO $ createDirectory outDir
  liftIO $ LB.writeFile genesisJSONFile (canonicalEncodePretty genesisData)

  dlgCerts <- mapM findDelegateCert . map ByronSigningKey $ gsRichSecrets gs

  liftIO $ wOut "genesis-keys" "key"
                serialiseToRawBytes
                (map ByronSigningKey $ gsDlgIssuersSecrets gs)
  liftIO $ wOut "delegate-keys" "key"
                serialiseToRawBytes
                (map ByronSigningKey $ gsRichSecrets gs)
  liftIO $ wOut "poor-keys" "key"
                serialiseToRawBytes
                (map (ByronSigningKey . Genesis.poorSecretToKey) $ gsPoorSecrets gs)
  liftIO $ wOut "delegation-cert" "json" serialiseDelegationCert dlgCerts
  liftIO $ wOut "avvm-secrets" "secret" printFakeAvvmSecrets $ gsFakeAvvmSecrets gs
 where
  dlgCertMap :: Map Common.KeyHash Certificate
  dlgCertMap = Genesis.unGenesisDelegation $ Genesis.gdHeavyDelegation genesisData

  findDelegateCert :: SigningKey ByronKey -> ExceptT ByronGenesisError IO Certificate
  findDelegateCert bSkey@(ByronSigningKey sk) =
    case find (isCertForSK sk) (Map.elems dlgCertMap) of
      Nothing -> left . NoGenesisDelegationForKey
                 . prettyPublicKey $ getVerificationKey bSkey
      Just x  -> right x

  genesisJSONFile :: FilePath
  genesisJSONFile = outDir <> "/genesis.json"

  printFakeAvvmSecrets :: Crypto.RedeemSigningKey -> ByteString
  printFakeAvvmSecrets rskey = encodeUtf8 . toStrict . toLazyText $ build rskey

  -- Compare a given 'SigningKey' with a 'Certificate' 'VerificationKey'
  isCertForSK :: Crypto.SigningKey -> Certificate -> Bool
  isCertForSK sk cert = delegateVK cert == Crypto.toVerification sk

  wOut :: String -> String -> (a -> ByteString) -> [a] -> IO ()
  wOut = writeSecrets outDir

writeSecrets :: FilePath -> String -> String -> (a -> ByteString) -> [a] -> IO ()
writeSecrets outDir prefix suffix secretOp xs =
  forM_ (zip xs [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    BS.writeFile filename $ secretOp secret
#ifdef UNIX
    setFileMode    filename ownerReadMode
#else
    setPermissions filename (emptyPermissions {readable = True})
#endif
