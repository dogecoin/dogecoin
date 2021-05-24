{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Cardano.Prelude
import           Hedgehog.Main (defaultMain)

#ifdef UNIX
import qualified Test.Cardano.Node.FilePermissions
#endif
import qualified Test.Cardano.Node.Json
import qualified Test.Cardano.Node.POM

main :: IO ()
main = defaultMain
#ifdef UNIX
  [ Test.Cardano.Node.Json.tests
  , Test.Cardano.Node.POM.tests
  , Test.Cardano.Node.FilePermissions.tests
  ]
#else
  [ Test.Cardano.Node.Json.tests
  , Test.Cardano.Node.POM.tests
  ]
#endif

