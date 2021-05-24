module Cardano.CLI.Byron.Parsers
  ( ByronCommand(..)
  , NodeCmd(..)
  , backwardsCompatibilityCommands
  , parseByronCommands
  , parseHeavyDelThd
  , parseInstallerHash
  , parseMaxBlockSize
  , parseMaxHeaderSize
  , parseMaxTxSize
  , parseMaxProposalSize
  , parseMpcThd
  , parseScriptVersion
  , parseSlotDuration
  , parseSoftforkRule
  , parseSystemTag
  , parseTxFeePolicy
  , parseUpdateProposalThd
  , parseUpdateProposalTTL
  , parseUnlockStakeEpoch
  , parseUpdateVoteThd
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Control.Monad (fail)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Attoparsec.Combinator ((<?>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Formatting (build, sformat)

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Cardano.Binary (Annotated (..))

import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Crypto.Hashing (hashRaw)
import           Cardano.Crypto.ProtocolMagic (AProtocolMagic (..), ProtocolMagic,
                   ProtocolMagicId (..))

import           Cardano.Chain.Common (BlockCount (..), TxFeePolicy (..), TxSizeLinear (..),
                   decodeAddressBase58, rationalToLovelacePortion)
import qualified Cardano.Chain.Common as Byron
import           Cardano.Chain.Genesis (FakeAvvmOptions (..), TestnetBalanceOptions (..))
import           Cardano.Chain.Slotting (EpochNumber (..), SlotNumber (..))
import           Cardano.Chain.Update (ApplicationName (..), InstallerHash (..), NumSoftwareVersion,
                   ProtocolVersion (..), SoftforkRule (..), SoftwareVersion (..), SystemTag (..),
                   checkApplicationName, checkSystemTag)

import           Cardano.Api hiding (UpdateProposal)
import           Cardano.Api.Byron (Address (..), ByronProtocolParametersUpdate (..), Lovelace (..),
                   toByronLovelace)

import           Cardano.CLI.Byron.Commands
import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Run (ClientCommand (ByronCommand))
import           Cardano.CLI.Shelley.Commands (ByronKeyFormat (..))
import           Cardano.CLI.Types

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper)
              $ mconcat [ progDesc descr ]

backwardsCompatibilityCommands :: Parser ClientCommand
backwardsCompatibilityCommands =
  asum hiddenCmds
 where
  convertToByronCommand :: Mod CommandFields ByronCommand -> Parser ClientCommand
  convertToByronCommand p = ByronCommand <$> Opt.subparser (p <> Opt.internal)

  hiddenCmds :: [Parser ClientCommand]
  hiddenCmds = map convertToByronCommand [ parseGenesisRelatedValues
                                         , parseKeyRelatedValues
                                         , parseTxRelatedValues
                                         , parseLocalNodeQueryValues
                                         , parseMiscellaneous
                                         ]

-- Implemented with asum so all commands don't get hidden when trying to hide
-- the 'pNodeCmdBackwardCompatible' parser.
parseByronCommands :: Parser ByronCommand
parseByronCommands = asum
  [ subParser "key" (Opt.info (Opt.subparser parseKeyRelatedValues)
      $ Opt.progDesc "Byron key utility commands")
  , subParser "transaction" (Opt.info (Opt.subparser parseTxRelatedValues)
      $ Opt.progDesc "Byron transaction commands")
  , subParser "query" (Opt.info (Opt.subparser parseLocalNodeQueryValues)
      $ Opt.progDesc "Byron node query commands.")
  , subParser "genesis" (Opt.info (Opt.subparser parseGenesisRelatedValues)
      $ Opt.progDesc "Byron genesis block commands")
  , subParser "governance" (Opt.info (NodeCmd <$> Opt.subparser pNodeCmd)
      $ Opt.progDesc "Byron governance commands")
  , subParser "miscellaneous" (Opt.info (Opt.subparser parseMiscellaneous)
      $ Opt.progDesc "Byron miscellaneous commands")
  , NodeCmd <$> pNodeCmdBackwardCompatible
  ]
 where
   subParser :: String -> ParserInfo ByronCommand -> Parser ByronCommand
   subParser name pInfo = Opt.subparser $ Opt.command name pInfo <> Opt.metavar name

pNodeCmdBackwardCompatible :: Parser NodeCmd
pNodeCmdBackwardCompatible = Opt.subparser $ pNodeCmd <> Opt.internal

parseCBORObject :: Parser CBORObject
parseCBORObject = asum
  [ CBORBlockByron <$> option auto
      (  long "byron-block"
      <> help
          (   "The CBOR file is a byron era block."
          <>  " Enter the number of slots in an epoch. The default value is 21600")
      <> metavar "INT"
      <> value (EpochSlots 21600)
      )

  , flag' CBORDelegationCertificateByron $
        long "byron-delegation-certificate"
     <> help "The CBOR file is a byron era delegation certificate"

  , flag' CBORTxByron $
        long "byron-tx"
     <> help "The CBOR file is a byron era tx"

  , flag' CBORUpdateProposalByron $
        long "byron-update-proposal"
     <> help "The CBOR file is a byron era update proposal"
  , flag' CBORVoteByron $
        long "byron-vote"
     <> help "The CBOR file is a byron era vote"
  ]

-- | Values required to create genesis.
parseGenesisParameters :: Parser GenesisParameters
parseGenesisParameters =
  GenesisParameters
    <$> parseUTCTime
          "start-time"
          "Start time of the new cluster to be enshrined in the new genesis."
    <*> parseFilePath
          "protocol-parameters-file"
          "JSON file with protocol parameters."
    <*> parseK
    <*> parseProtocolMagic
    <*> parseTestnetBalanceOptions
    <*> parseFakeAvvmOptions
    <*> (rationalToLovelacePortion <$>
         parseFractionWithDefault
          "avvm-balance-factor"
          "AVVM balances will be multiplied by this factor (defaults to 1)."
          1)
    <*> optional
        ( parseIntegral
            "secret-seed"
            "Optionally specify the seed of generation."
        )

parseGenesisRelatedValues :: Mod CommandFields ByronCommand
parseGenesisRelatedValues =
  mconcat
    [ command' "genesis" "Create genesis."
      $ Genesis
          <$> parseNewDirectory
              "genesis-output-dir"
              "Non-existent directory where genesis JSON file and secrets shall be placed."
          <*> parseGenesisParameters
    , command' "print-genesis-hash" "Compute hash of a genesis file."
        $ PrintGenesisHash
            <$> parseGenesisFile "genesis-json"
    ]

-- | Values required to create keys and perform
-- transformation on keys.
parseKeyRelatedValues :: Mod CommandFields ByronCommand
parseKeyRelatedValues =
    mconcat
        [ command' "keygen" "Generate a signing key."
            $ Keygen
                <$> parseNewSigningKeyFile "secret"
        , command'
            "to-verification"
            "Extract a verification key in its base64 form."
            $ ToVerification
                <$> parseByronKeyFormat
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key file to extract the verification part from."
                <*> parseNewVerificationKeyFile "to"
        , command'
            "signing-key-public"
            "Pretty-print a signing key's verification key (not a secret)."
            $ PrettySigningKeyPublic
                <$> parseByronKeyFormat
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key to pretty-print."
        , command'
            "signing-key-address"
            "Print address of a signing key."
            $ PrintSigningKeyAddress
                <$> parseByronKeyFormat
                <*> pNetworkId
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key, whose address is to be printed."
        , command'
            "migrate-delegate-key-from"
            "Migrate a delegate key from an older version."
            $ MigrateDelegateKeyFrom
                <$> parseSigningKeyFile "from" "Legacy signing key file to migrate."
                <*> parseNewSigningKeyFile "to"
        ]

parseLocalNodeQueryValues :: Mod CommandFields ByronCommand
parseLocalNodeQueryValues =
    mconcat
        [ command' "get-tip" "Get the tip of your local node's blockchain"
            $ GetLocalNodeTip
                <$> pNetworkId
        ]

parseMiscellaneous :: Mod CommandFields ByronCommand
parseMiscellaneous = mconcat
  [ command'
      "validate-cbor"
      "Validate a CBOR blockchain object."
      $ ValidateCBOR
          <$> parseCBORObject
          <*> parseFilePath "filepath" "Filepath of CBOR file."
  , command'
      "pretty-print-cbor"
      "Pretty print a CBOR file."
      $ PrettyPrintCBOR
          <$> parseFilePath "filepath" "Filepath of CBOR file."
  ]



parseTestnetBalanceOptions :: Parser TestnetBalanceOptions
parseTestnetBalanceOptions =
  TestnetBalanceOptions
    <$> parseIntegral
          "n-poor-addresses"
          "Number of poor nodes (with small balance)."
    <*> parseIntegral
          "n-delegate-addresses"
          "Number of delegate nodes (with huge balance)."
    <*> parseLovelace
          "total-balance"
          "Total balance owned by these nodes."
    <*> parseFraction
          "delegate-share"
          "Portion of stake owned by all delegates together."

parseTxIn :: Parser TxIn
parseTxIn =
  option
  (readerFromAttoParser parseTxInAtto)
  $ long "txin"
    <> metavar "(TXID,INDEX)"
    <> help "Transaction input is a pair of an UTxO TxId and a zero-based output index."

parseTxInAtto :: Atto.Parser TxIn
parseTxInAtto =
  TxIn <$> (Atto.char '(' *> parseTxIdAtto <* Atto.char ',')
       <*> (parseTxIxAtto <* Atto.char ')')


parseTxIdAtto :: Atto.Parser TxId
parseTxIdAtto = (<?> "Transaction ID (hexadecimal)") $ do
  bstr <- Atto.takeWhile1 Char.isHexDigit
  case deserialiseFromRawBytesHex AsTxId bstr of
    Just addr -> return addr
    Nothing -> fail $ "Incorrect transaction id format:: " ++ show bstr

parseTxIxAtto :: Atto.Parser TxIx
parseTxIxAtto = toEnum <$> Atto.decimal

parseTxOut :: Parser (TxOut ByronEra)
parseTxOut =
  option
    ( (\(addr, lovelace) -> TxOut (pAddressInEra addr)
                                  (pLovelaceTxOut lovelace)
                                  TxOutDatumHashNone)
      <$> auto
    )
    $ long "txout"
      <> metavar "'(\"ADDR\", LOVELACE)'"
      <> help "Specify a transaction output, as a pair of an address and lovelace."
 where
  pAddressInEra :: Text -> AddressInEra ByronEra
  pAddressInEra t =
    case decodeAddressBase58 t of
      Left err -> panic $ "Bad Base58 address: " <> Text.pack (show err)
      Right byronAddress -> AddressInEra ByronAddressInAnyEra $ ByronAddress byronAddress

  pLovelaceTxOut :: Word64 -> TxOutValue ByronEra
  pLovelaceTxOut l =
    if l > (maxBound :: Word64)
    then panic $ show l <> " lovelace exceeds the Word64 upper bound"
    else TxOutAdaOnly AdaOnlyInByronEra . Lovelace $ toInteger l

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
  Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

parseTxRelatedValues :: Mod CommandFields ByronCommand
parseTxRelatedValues =
  mconcat
    [ command'
        "submit-tx"
        "Submit a raw, signed transaction, in its on-wire representation."
        $ SubmitTx
            <$> pNetworkId
            <*> parseTxFile "tx"
    , command'
        "issue-genesis-utxo-expenditure"
        "Write a file with a signed transaction, spending genesis UTxO."
        $ SpendGenesisUTxO
            <$> parseGenesisFile "genesis-json"
            <*> pNetworkId
            <*> parseByronKeyFormat
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> parseAddress
                  "rich-addr-from"
                  "Tx source: genesis UTxO richman address (non-HD)."
            <*> some parseTxOut

    , command'
        "issue-utxo-expenditure"
        "Write a file with a signed transaction, spending normal UTxO."
        $ SpendUTxO
            <$> pNetworkId
            <*> parseByronKeyFormat
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> some parseTxIn
            <*> some parseTxOut

    , command'
        "txid"
        "Print the txid of a raw, signed transaction."
        $ GetTxId
            <$> parseTxFile "tx"
    ]

pNodeCmd :: Mod CommandFields NodeCmd
pNodeCmd =
    mconcat
      [ Opt.command "create-update-proposal"
          (Opt.info parseByronUpdateProposal $ Opt.progDesc  "Create an update proposal.")

      , Opt.command "create-proposal-vote"
          (Opt.info parseByronVote $ Opt.progDesc "Create an update proposal vote.")

      , Opt.command "submit-update-proposal"
          (Opt.info parseByronUpdateProposalSubmission $ Opt.progDesc "Submit an update proposal.")

      , Opt.command "submit-proposal-vote"
          (Opt.info parseByronVoteSubmission $ Opt.progDesc "Submit a proposal vote.")
      ]

parseByronUpdateProposal :: Parser NodeCmd
parseByronUpdateProposal = do
  UpdateProposal
    <$> pNetworkId
    <*> parseSigningKeyFile "signing-key" "Path to signing key."
    <*> parseProtocolVersion
    <*> parseSoftwareVersion
    <*> parseSystemTag
    <*> parseInstallerHash
    <*> parseFilePath "filepath" "Byron proposal output filepath."
    <*> pByronProtocolParametersUpdate

parseByronVoteSubmission :: Parser NodeCmd
parseByronVoteSubmission = do
  SubmitVote
    <$> pNetworkId
    <*> parseFilePath "filepath" "Filepath of Byron update proposal vote."


pByronProtocolParametersUpdate :: Parser ByronProtocolParametersUpdate
pByronProtocolParametersUpdate =
  ByronProtocolParametersUpdate
    <$> optional parseScriptVersion
    <*> optional parseSlotDuration
    <*> optional parseMaxBlockSize
    <*> optional parseMaxHeaderSize
    <*> optional parseMaxTxSize
    <*> optional parseMaxProposalSize
    <*> optional parseMpcThd
    <*> optional parseHeavyDelThd
    <*> optional parseUpdateVoteThd
    <*> optional parseUpdateProposalThd
    <*> optional parseUpdateProposalTTL
    <*> optional parseSoftforkRule
    <*> optional parseTxFeePolicy
    <*> optional parseUnlockStakeEpoch

parseByronUpdateProposalSubmission :: Parser NodeCmd
parseByronUpdateProposalSubmission =
  SubmitUpdateProposal
    <$> pNetworkId
    <*> parseFilePath "filepath" "Filepath of Byron update proposal."


parseByronVote :: Parser NodeCmd
parseByronVote =
  CreateVote
    <$> pNetworkId
    <*> (SigningKeyFile <$> parseFilePath "signing-key" "Filepath of signing key.")
    <*> parseFilePath "proposal-filepath" "Filepath of Byron update proposal."
    <*> parseVoteBool
    <*> parseFilePath "output-filepath" "Byron vote output filepath."

--------------------------------------------------------------------------------
-- CLI Parsers
--------------------------------------------------------------------------------

parseScriptVersion :: Parser Word16
parseScriptVersion =
  option auto
    ( long "script-version"
    <> metavar "WORD16"
    <> help "Proposed script version."
    )

parseSlotDuration :: Parser Natural
parseSlotDuration =
  option auto
    ( long "slot-duration"
    <> metavar "NATURAL"
    <> help "Proposed slot duration."
    )

parseSystemTag :: Parser SystemTag
parseSystemTag = option (eitherReader checkSysTag)
                   ( long "system-tag"
                   <> metavar "STRING"
                   <> help "Identify which system (linux, win64, etc) the update proposal is for."
                   )
 where
  checkSysTag :: String -> Either String SystemTag
  checkSysTag name =
    let tag = SystemTag $ toS name
    in case checkSystemTag tag of
         Left err -> Left . toS $ sformat build err
         Right () -> Right tag

parseInstallerHash :: Parser InstallerHash
parseInstallerHash =
  InstallerHash .  hashRaw . C8.pack
    <$> strOption ( long "installer-hash"
                  <> metavar "HASH"
                  <> help "Software hash."
                  )

parseMaxBlockSize :: Parser Natural
parseMaxBlockSize =
  option auto
    ( long "max-block-size"
    <> metavar "NATURAL"
    <> help "Proposed max block size."
    )

parseMaxHeaderSize :: Parser Natural
parseMaxHeaderSize =
  option auto
    ( long "max-header-size"
    <> metavar "NATURAL"
    <> help "Proposed max block header size."
    )

parseMaxTxSize :: Parser Natural
parseMaxTxSize =
  option auto
    ( long "max-tx-size"
    <> metavar "NATURAL"
    <> help "Proposed max transaction size."
    )

parseMaxProposalSize :: Parser  Natural
parseMaxProposalSize =
  option auto
    ( long "max-proposal-size"
    <> metavar "NATURAL"
    <> help "Proposed max update proposal size."
    )

parseMpcThd :: Parser Byron.LovelacePortion
parseMpcThd =
  rationalToLovelacePortion
    <$> parseFraction "max-mpc-thd" "Proposed max mpc threshold."

parseProtocolVersion :: Parser ProtocolVersion
parseProtocolVersion =
  ProtocolVersion <$> (parseWord "protocol-version-major" "Protocol verson major." "WORD16" :: Parser Word16)
                  <*> (parseWord "protocol-version-minor" "Protocol verson minor." "WORD16" :: Parser Word16)
                  <*> (parseWord "protocol-version-alt" "Protocol verson alt." "WORD8" :: Parser Word8)

parseHeavyDelThd :: Parser Byron.LovelacePortion
parseHeavyDelThd =
  rationalToLovelacePortion
    <$> parseFraction "heavy-del-thd" "Proposed heavy delegation threshold."

parseUpdateVoteThd :: Parser Byron.LovelacePortion
parseUpdateVoteThd =
  rationalToLovelacePortion
    <$> parseFraction "update-vote-thd" "Propose update vote threshold."

parseUpdateProposalThd :: Parser Byron.LovelacePortion
parseUpdateProposalThd =
  rationalToLovelacePortion
    <$> parseFraction "update-proposal-thd" "Propose update proposal threshold."

parseUpdateProposalTTL :: Parser SlotNumber
parseUpdateProposalTTL =
  SlotNumber
    <$> option auto
          ( long "time-to-live"
          <> metavar "WORD64"
          <> help "Proposed time for an update proposal to live."
          )

parseSoftforkRule :: Parser SoftforkRule
parseSoftforkRule =
  SoftforkRule
    <$> (rationalToLovelacePortion <$> parseFraction "softfork-init-thd" "Propose initial threshold (right after proposal is confirmed).")
    <*> (rationalToLovelacePortion <$> parseFraction "softfork-min-thd" "Propose minimum threshold (threshold can't be less than this).")
    <*> (rationalToLovelacePortion <$> parseFraction "softfork-thd-dec" "Propose threshold decrement (threshold will decrease by this amount after each epoch).")


parseSoftwareVersion :: Parser SoftwareVersion
parseSoftwareVersion =
  SoftwareVersion <$> parseApplicationName <*> parseNumSoftwareVersion

parseApplicationName :: Parser ApplicationName
parseApplicationName = option (eitherReader checkAppNameLength)
       (  long "application-name"
       <> metavar "STRING"
       <> help "The name of the application."
       )
 where
  checkAppNameLength :: String -> Either String ApplicationName
  checkAppNameLength name =
    let appName = ApplicationName $ toS name
    in case checkApplicationName appName of
         Left err -> Left . toS $ sformat build err
         Right () -> Right appName

parseNumSoftwareVersion :: Parser NumSoftwareVersion
parseNumSoftwareVersion =
  parseWord
    "software-version-num"
    "Numeric software version associated with application name."
    "WORD32"

parseTxFeePolicy :: Parser TxFeePolicy
parseTxFeePolicy =
  TxFeePolicyTxSizeLinear
    <$> ( TxSizeLinear <$> parseLovelace "tx-fee-a-constant" "Propose the constant a for txfee = a + b*s where s is the size."
                       <*> parseFraction "tx-fee-b-constant" "Propose the constant b for txfee = a + b*s where s is the size."
        )

parseVoteBool :: Parser Bool
parseVoteBool = flag' True (long "vote-yes" <> help "Vote yes with respect to an update proposal.")
            <|> flag' False (long "vote-no" <> help "Vote no with respect to an update proposal.")

parseUnlockStakeEpoch :: Parser EpochNumber
parseUnlockStakeEpoch =
  EpochNumber
    <$> option auto
      ( long "unlock-stake-epoch"
      <> metavar "WORD64"
      <> help "Proposed epoch to unlock all stake."
      )


parseWord :: Integral a => String -> String -> String -> Parser a
parseWord optname desc metvar = option (fromInteger <$> auto)
  $ long optname <> metavar metvar <> help desc



parseAddress :: String -> String -> Parser (Address ByronAddr)
parseAddress opt desc =
  option (cliParseBase58Address <$> str)
    $ long opt <> metavar "ADDR" <> help desc

parseByronKeyFormat :: Parser ByronKeyFormat
parseByronKeyFormat = asum
  [ flag' LegacyByronKeyFormat $
        long "byron-legacy-formats"
     <> help "Byron/cardano-sl formats and compatibility"

  , flag' NonLegacyByronKeyFormat $
        long "byron-formats"
     <> help "Byron era formats and compatibility"

    -- And hidden compatibility flag aliases that should be deprecated:
  , flag' LegacyByronKeyFormat $ hidden <> long "byron-legacy"
  , flag' NonLegacyByronKeyFormat $ hidden <> long "real-pbft"

  -- Default Byron key format
  , pure NonLegacyByronKeyFormat
  ]


parseFakeAvvmOptions :: Parser FakeAvvmOptions
parseFakeAvvmOptions =
  FakeAvvmOptions
    <$> parseIntegral "avvm-entry-count" "Number of AVVM addresses."
    <*> parseLovelace "avvm-entry-balance" "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
    <$> parseIntegral "k" "The security parameter of the Ouroboros protocol."

parseNewDirectory :: String -> String -> Parser NewDirectory
parseNewDirectory opt desc = NewDirectory <$> parseFilePath opt desc

parseFractionWithDefault
  :: String
  -> String
  -> Double
  -> Parser Rational
parseFractionWithDefault optname desc w =
  toRational <$> option readDouble
    ( long optname
    <> metavar "DOUBLE"
    <> help desc
    <> value w
    )

pNetworkId :: Parser NetworkId
pNetworkId =
  pMainnet' <|> fmap Testnet pTestnetMagic
 where
   pMainnet' :: Parser NetworkId
   pMainnet' =
    Opt.flag' Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

pTestnetMagic :: Parser NetworkMagic
pTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

parseNewSigningKeyFile :: String -> Parser NewSigningKeyFile
parseNewSigningKeyFile opt =
  NewSigningKeyFile
    <$> parseFilePath opt "Non-existent file to write the signing key to."

parseNewTxFile :: String -> Parser NewTxFile
parseNewTxFile opt =
  NewTxFile
    <$> parseFilePath opt "Non-existent file to write the signed transaction to."

parseNewVerificationKeyFile :: String -> Parser NewVerificationKeyFile
parseNewVerificationKeyFile opt =
  NewVerificationKeyFile
    <$> parseFilePath opt "Non-existent file to write the verification key to."

parseProtocolMagicId :: String -> Parser ProtocolMagicId
parseProtocolMagicId arg =
  ProtocolMagicId
    <$> parseIntegral arg "The magic number unique to any instance of Cardano."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated ()
    <$> parseProtocolMagicId "protocol-magic"

parseTxFile :: String -> Parser TxFile
parseTxFile opt =
  TxFile
    <$> parseFilePath opt "File containing the signed transaction."

parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
  option (posixSecondsToUTCTime . fromInteger <$> auto)
    $ long optname <> metavar "POSIXSECONDS" <> help desc

cliParseBase58Address :: Text -> Address ByronAddr
cliParseBase58Address t =
  case decodeAddressBase58 t of
    Left err -> panic $ "Bad Base58 address: " <> Text.pack (show err)
    Right byronAddress -> ByronAddress byronAddress

parseFraction :: String -> String -> Parser Rational
parseFraction optname desc =
  option (toRational <$> readDouble) $
      long optname
   <> metavar "DOUBLE"
   <> help desc

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc

parseLovelace :: String -> String -> Parser Byron.Lovelace
parseLovelace optname desc =
  Opt.option (readerFromAttoParser parseLovelaceAtto)
    (  long optname
    <> metavar "INT"
    <> help desc
    )
 where
  parseLovelaceAtto :: Atto.Parser Byron.Lovelace
  parseLovelaceAtto = do
    i <- Atto.decimal
    if i > toInteger (maxBound :: Word64)
    then fail $ show i <> " lovelace exceeds the Word64 upper bound"
    else case toByronLovelace (Lovelace i) of
           Just byronLovelace -> return byronLovelace
           Nothing -> panic $ "Error converting lovelace: " <> Text.pack (show i)

readDouble :: ReadM Double
readDouble = do
  f <- auto
  when (f < 0) $ readerError "fraction must be >= 0"
  when (f > 1) $ readerError "fraction must be <= 1"
  return f

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption
    ( long optname
    <> metavar "FILEPATH"
    <> help desc
    <> completer (bashCompleter "file")
    )

parseSigningKeyFile :: String -> String -> Parser SigningKeyFile
parseSigningKeyFile opt desc = SigningKeyFile <$> parseFilePath opt desc


parseGenesisFile :: String -> Parser GenesisFile
parseGenesisFile opt =
  GenesisFile <$> parseFilePath opt "Genesis JSON file."

