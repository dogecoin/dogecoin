module Test.OptParse
  ( checkTextEnvelopeFormat
  , equivalence
  , execCardanoCLI
  , propertyOnce
  , withSnd
  , noteInputFile
  , noteTempFile
  ) where

import           Cardano.Prelude hiding (lines, readFile, stderr, stdout)
import           Prelude (String)
import qualified Prelude

import           Control.Monad.Catch
import qualified GHC.Stack as GHC

import           Cardano.Api

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Process as H
import           Hedgehog.Internal.Property (Diff, MonadTest, liftTest, mkTest)
import qualified Hedgehog.Internal.Property as H
import           Hedgehog.Internal.Show (ValueDiff (ValueSame), mkValue, showPretty, valueDiff)
import           Hedgehog.Internal.Source (getCaller)


-- | Execute cardano-cli via the command line.
--
-- Waits for the process to finish and returns the stdout.
execCardanoCLI
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m String
  -- ^ Captured stdout
execCardanoCLI = GHC.withFrozenCallStack $ H.execFlex "cardano-cli" "CARDANO_CLI"

-- | Checks that the 'tvType' and 'tvDescription' are equivalent between two files.
checkTextEnvelopeFormat
  :: (MonadTest m, MonadIO m, HasCallStack)
  => TextEnvelopeType
  -> FilePath
  -> FilePath
  -> m ()
checkTextEnvelopeFormat tve reference created = do
  eRefTextEnvelope <- liftIO $ readTextEnvelopeOfTypeFromFile tve reference
  refTextEnvelope <- handleTextEnvelope eRefTextEnvelope

  eCreatedTextEnvelope <- liftIO $ readTextEnvelopeOfTypeFromFile tve created
  createdTextEnvelope <- handleTextEnvelope eCreatedTextEnvelope

  typeTitleEquivalence refTextEnvelope createdTextEnvelope
 where
   handleTextEnvelope :: MonadTest m
                      => Either (FileError TextEnvelopeError) TextEnvelope
                      -> m TextEnvelope
   handleTextEnvelope (Right refTextEnvelope) = return refTextEnvelope
   handleTextEnvelope (Left fileErr) = failWithCustom callStack Nothing . displayError $ fileErr

   typeTitleEquivalence :: MonadTest m => TextEnvelope -> TextEnvelope -> m ()
   typeTitleEquivalence (TextEnvelope refType refTitle _)
                        (TextEnvelope createdType createdTitle _) = do
     equivalence refType createdType
     equivalence refTitle createdTitle

--------------------------------------------------------------------------------
-- Helpers, Error rendering & Clean up
--------------------------------------------------------------------------------

cardanoCliPath :: FilePath
cardanoCliPath = "cardano-cli"

-- | Return the input file path after annotating it relative to the project root directory
noteInputFile :: (MonadTest m, HasCallStack) => FilePath -> m FilePath
noteInputFile filePath = withFrozenCallStack $ do
  H.annotate $ cardanoCliPath <> "/" <> filePath
  return filePath

-- | Return the test file path after annotating it relative to the project root directory
noteTempFile :: (MonadTest m, HasCallStack) => FilePath -> FilePath -> m FilePath
noteTempFile tempDir filePath = withFrozenCallStack $ do
  let relPath = tempDir <> "/" <> filePath
  H.annotate $ cardanoCliPath <> "/" <> relPath
  return relPath

-- | Return the supply value with the result of the supplied function as a tuple
withSnd :: (a -> b) -> a -> (a, b)
withSnd f a = (a, f a)

-- These were lifted from hedgehog and slightly modified

propertyOnce :: H.PropertyT IO () -> H.Property
propertyOnce =  H.withTests 1 . H.withShrinks 0 . H.property

-- | Check for equivalence between two types and perform a file cleanup on failure.
equivalence
  :: (MonadTest m, Eq a, Show a, HasCallStack)
  => a
  -> a
  -> m ()
equivalence x y = do
  ok <- H.eval (x == y)
  if ok
    then H.success
    else failDiffCustom callStack x y

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg =
  liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)

-- | Fails with an error that shows the difference between two values.
failDiffCustom :: (MonadTest m, Show a) => CallStack -> a -> a -> m ()
failDiffCustom cS x y =
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      withFrozenCallStack $
        failWithCustom cS Nothing $
        Prelude.unlines [
            "Failed"
          , "━━ lhs ━━"
          , showPretty x
          , "━━ rhs ━━"
          , showPretty y
          ]

    Just vdiff@(ValueSame _) ->
      withFrozenCallStack $
        failWithCustom cS (Just $
          H.Diff "━━━ Failed ("  "" "no differences" "" ") ━━━" vdiff) ""

    Just vdiff ->
      withFrozenCallStack $
        failWithCustom cS (Just $
          H.Diff "━━━ Failed (" "- lhs" ") (" "+ rhs" ") ━━━" vdiff) ""
