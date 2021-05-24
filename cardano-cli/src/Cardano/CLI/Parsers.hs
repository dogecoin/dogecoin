module Cardano.CLI.Parsers
  ( opts
  , pref
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Cardano.CLI.Byron.Parsers (backwardsCompatibilityCommands, parseByronCommands)
import           Cardano.CLI.Run (ClientCommand (..))
import           Cardano.CLI.Shelley.Parsers (parseShelleyCommands)

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper)
              $ mconcat [ progDesc descr ]

opts :: ParserInfo ClientCommand
opts =
  Opt.info (parseClientCommand <**> Opt.helper)
    ( Opt.fullDesc
      <> Opt.header
      "cardano-cli - utility to support a variety of key\
      \ operations (genesis generation, migration,\
      \ pretty-printing..) for different system generations."
    )

pref :: ParserPrefs
pref = Opt.prefs showHelpOnEmpty

parseClientCommand :: Parser ClientCommand
parseClientCommand =
  asum
    -- There are name clashes between Shelley commands and the Byron backwards
    -- compat commands (e.g. "genesis"), and we need to prefer the Shelley ones
    -- so we list it first.
    [ parseShelley
    , parseByron
    , parseDeprecatedShelleySubcommand
    , backwardsCompatibilityCommands
    , parseDisplayVersion
    ]

parseByron :: Parser ClientCommand
parseByron =
  fmap ByronCommand $
  subparser $ mconcat
    [ commandGroup "Byron specific commands"
    , metavar "Byron specific commands"
    , command'
        "byron"
        "Byron specific commands"
         parseByronCommands
    ]

-- | Parse Shelley-related commands at the top level of the CLI.
parseShelley :: Parser ClientCommand
parseShelley = ShelleyCommand <$> parseShelleyCommands

-- | Parse Shelley-related commands under the now-deprecated \"shelley\"
-- subcommand.
--
-- Note that this subcommand is 'internal' and is therefore hidden from the
-- help text.
parseDeprecatedShelleySubcommand :: Parser ClientCommand
parseDeprecatedShelleySubcommand =
  subparser $ mconcat
    [ commandGroup "Shelley specific commands (deprecated)"
    , metavar "Shelley specific commands"
    , command'
        "shelley"
        "Shelley specific commands (deprecated)"
        (DeprecatedShelleySubcommand <$> parseShelleyCommands)
    , internal
    ]

-- Yes! A --version flag or version command. Either guess is right!
parseDisplayVersion :: Parser ClientCommand
parseDisplayVersion =
      subparser
        (mconcat
         [ commandGroup "Miscellaneous commands"
         , metavar "Miscellaneous commands"
         , command'
           "version"
           "Show the cardano-cli version"
           (pure DisplayVersion)
         ]
        )
  <|> flag' DisplayVersion
        (  long "version"
        <> help "Show the cardano-cli version"
        <> hidden
        )
