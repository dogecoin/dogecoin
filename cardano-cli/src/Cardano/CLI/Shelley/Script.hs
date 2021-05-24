module Cardano.CLI.Shelley.Script
  ( ScriptDecodeError (..)
  , deserialiseScriptInAnyLang
  , readFileScriptInAnyLang
  ) where

import           Prelude
import           Cardano.Prelude (ExceptT)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither)

import           Cardano.Api



--
-- Handling decoding the variety of script languages and formats
--

data ScriptDecodeError =
       ScriptDecodeTextEnvelopeError TextEnvelopeError
     | ScriptDecodeSimpleScriptError JsonDecodeError
  deriving Show

instance Error ScriptDecodeError where
  displayError (ScriptDecodeTextEnvelopeError err) =
    "Error decoding script: " ++ displayError err
  displayError (ScriptDecodeSimpleScriptError err) =
    "Syntax error in script: " ++ displayError err


-- | Read a script file. The file can either be in the text envelope format
-- wrapping the binary representation of any of the supported script languages,
-- or alternatively it can be a JSON format file for one of the simple script
-- language versions.
--
readFileScriptInAnyLang :: FilePath
                        -> ExceptT (FileError ScriptDecodeError) IO
                                   ScriptInAnyLang
readFileScriptInAnyLang file = do
    scriptBytes <- handleIOExceptT (FileIOError file) $ BS.readFile file
    firstExceptT (FileError file) $ hoistEither $
      deserialiseScriptInAnyLang scriptBytes


deserialiseScriptInAnyLang :: ByteString
                           -> Either ScriptDecodeError ScriptInAnyLang
deserialiseScriptInAnyLang bs =
    -- Accept either the text envelope format wrapping the binary serialisation,
    -- or accept the simple script language in its JSON format.
    --
    case deserialiseFromJSON AsTextEnvelope bs of
      Left _   ->
        -- The SimpleScript language has the property that it is backwards
        -- compatible, so we can parse as the latest version and then downgrade
        -- to the minimum version that has all the features actually used.
        case deserialiseFromJSON (AsSimpleScript AsSimpleScriptV2) bs of
          Left  err    -> Left (ScriptDecodeSimpleScriptError err)
          Right script -> Right (toMinimumSimpleScriptVersion script)

      Right te ->
        case deserialiseFromTextEnvelopeAnyOf textEnvTypes te of
          Left  err    -> Left (ScriptDecodeTextEnvelopeError err)
          Right script -> Right script

  where
    textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
    textEnvTypes =
      [ FromSomeType (AsScript AsSimpleScriptV1)
                     (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1))

      , FromSomeType (AsScript AsSimpleScriptV2)
                     (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2))

      , FromSomeType (AsScript AsPlutusScriptV1)
                     (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1))
      ]

    toMinimumSimpleScriptVersion :: SimpleScript SimpleScriptV2
                                 -> ScriptInAnyLang
    toMinimumSimpleScriptVersion s =
      -- TODO alonzo: this will need to be adjusted when more versions are added
      -- with appropriate helper functions it can probably be done in an
      -- era-generic style
      case adjustSimpleScriptVersion SimpleScriptV1 s of
        Nothing -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2)
                                   (SimpleScript SimpleScriptV2 s)
        Just s' -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1)
                                   (SimpleScript SimpleScriptV1 s')
