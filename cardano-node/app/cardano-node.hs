{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)
import qualified Data.Text as Text
import           Prelude (String)

import           Options.Applicative
import qualified Options.Applicative as Opt
import           Options.Applicative.Help ((<$$>))

import           Cardano.Config.Git.Rev (gitRev)
import           Data.Version (showVersion)
import           Paths_cardano_node (version)
import           System.Info (arch, compilerName, compilerVersion, os)

import           Cardano.Node.Configuration.POM (PartialNodeConfiguration)
import           Cardano.Node.Handlers.TopLevel
import           Cardano.Node.Parsers (nodeCLIParser, parserHelpHeader, parserHelpOptions,
                   renderHelpDoc)
import           Cardano.Node.Run (runNode)

main :: IO ()
main = toplevelExceptionHandler $ do

    cmd <- Opt.customExecParser p opts

    case cmd of
      RunCmd args -> runRunCommand args
      VersionCmd  -> runVersionCommand

    where
      p = Opt.prefs Opt.showHelpOnEmpty

      opts :: Opt.ParserInfo Command
      opts =
        Opt.info (fmap RunCmd nodeCLIParser <|> parseVersionCmd
                    <**> helperBrief "help" "Show this help text" nodeCliHelpMain)

          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )

      helperBrief :: String -> String -> String -> Parser (a -> a)
      helperBrief l d helpText = Opt.abortOption (Opt.InfoMsg helpText) $ mconcat
        [ Opt.long l
        , Opt.help d ]

      nodeCliHelpMain :: String
      nodeCliHelpMain = renderHelpDoc 80 $
        parserHelpHeader "cardano-node" nodeCLIParser
        <$$> ""
        <$$> parserHelpOptions nodeCLIParser


data Command = RunCmd PartialNodeConfiguration
             | VersionCmd

-- Yes! A --version flag or version command. Either guess is right!
parseVersionCmd :: Parser Command
parseVersionCmd =
      Opt.subparser
        (mconcat
         [ Opt.commandGroup "Miscellaneous commands"
         , Opt.metavar "version"
         , Opt.hidden
         , command'
           "version"
           "Show the cardano-node version"
           (pure VersionCmd)
         ]
        )
  <|> Opt.flag' VersionCmd
        (  Opt.long "version"
        <> Opt.help "Show the cardano-node version"
        <> Opt.hidden
        )

runVersionCommand :: IO ()
runVersionCommand =
    putTextLn $ mconcat
      [ "cardano-node ", renderVersion version
      , " - ", Text.pack os, "-", Text.pack arch
      , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
      , "\ngit rev ", gitRev
      ]
  where
    renderVersion = Text.pack . showVersion


runRunCommand :: PartialNodeConfiguration -> IO ()
runRunCommand pnc = liftIO $ runNode pnc

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper)
              $ mconcat [ progDesc descr ]
