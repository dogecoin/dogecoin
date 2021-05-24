module Cardano.Config.Git.RevFromGit (
      gitRevFromGit
    ) where

import           Cardano.Prelude
import           Prelude (String)

import qualified Language.Haskell.TH as TH
import           System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import           System.Process (readProcessWithExitCode)

-- | Git revision found by running git rev-parse. If git could not be
-- executed, then this will be an empty string.
gitRevFromGit :: TH.Q TH.Exp
gitRevFromGit = TH.LitE . TH.StringL <$> TH.runIO runGitRevParse
    where
        runGitRevParse :: IO String
        runGitRevParse = handleJust missingGit (const $ pure "") $ do
            (exitCode, output, _) <-
                readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
            pure $ case exitCode of
                ExitSuccess -> output
                _           -> ""

        missingGit e = if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing
