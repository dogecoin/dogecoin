{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Config.Git.Rev (
      gitRev
    ) where

import           Cardano.Prelude

import           Data.FileEmbed (dummySpaceWith)
import qualified Data.Text as T

import           Cardano.Config.Git.RevFromGit (gitRevFromGit)

gitRev :: Text
gitRev | gitRevEmbed /= zeroRev = gitRevEmbed
       | T.null fromGit         = zeroRev
       | otherwise              = fromGit
    where
        -- Git revision embedded after compilation using
        -- Data.FileEmbed.injectWith. If nothing has been injected,
        -- this will be filled with 0 characters.
        gitRevEmbed :: Text
        gitRevEmbed = decodeUtf8 $(dummySpaceWith "gitrev" 40)

        -- Git revision found during compilation by running git. If
        -- git could not be run, then this will be empty.
#if defined(arm_HOST_ARCH)
        -- cross compiling to arm fails; due to a linker bug
        fromGit = ""
#else
        fromGit = T.strip (T.pack $(gitRevFromGit))
#endif

zeroRev :: Text
zeroRev = "0000000000000000000000000000000000000000"
