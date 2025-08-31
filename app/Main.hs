module Main (main) where

import Control.Monad (forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Domain
import FileSystem
import Options.Applicative
import System.Exit (exitFailure)

main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnEmpty) commandInfo
  case cmd of
    Run {..} -> do
      parsed <- parseLinks targets
      let checked = checkLinks parsed
          broken = filterBrokenLinks checked
          summary = mkSummary parsed checked broken

      case verbosity of
        LogMuted -> return ()
        LogSummary ->
          case format of
            Plain -> do
              when (hasBrokenLinks broken) $ do
                putStrLn "# Broken links"

                forM_ broken.unBrokenLinks $ \withRoot ->
                  forM_ (Map.toList withRoot.links) $ \(file, links) ->
                    forM_ links $ \link ->
                      putStrLn $ file <> ":" <> T.unpack (originalLink link)

                putStrLn ""

              putStrLn $
                "Run over "
                  <> show summary.parsing.remoteBaseURLs
                  <> " base URLs"
                  <> " and "
                  <> show summary.parsing.files
                  <> " files,"
                  <> " only "
                  <> show summary.broken.links
                  <> " links where broken out of "
                  <> show summary.parsing.links
            Json ->
              LBS.putStr $ Aeson.encode summary
        LogDetailed ->
          LBS.putStr $
            Aeson.encode $
              Aeson.object
                [ "parsing" Aeson..= parsed,
                  "checking" Aeson..= checked,
                  "broken" Aeson..= broken,
                  "summary" Aeson..= summary
                ]

      when (hasBrokenLinks broken) exitFailure

-- * CLI

data Command
  = Run {verbosity :: Verbosity, format :: Format, targets :: [(RemoteBaseURL, Folder)]}

data Verbosity
  = LogMuted
  | LogSummary
  | LogDetailed
  deriving stock (Eq, Show)

data Format
  = Plain
  | Json
  deriving stock (Eq, Show)

commandInfo :: ParserInfo Command
commandInfo =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> header hopHeader
        <> progDesc hopDesc
    )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "run"
        ( info
            (Run <$> verbosityP <*> formatP <*> many targetP)
            (progDesc "Look for broken links")
        )
    )
  where
    verbosityP =
      flag'
        LogSummary
        ( long "verbose"
            <> short 'v'
            <> help "Display summary"
        )
        <|> flag'
          LogDetailed
          ( long "very-verbose"
              <> short 'V'
              <> help "Display summary and each phase details (json)"
          )
        <|> pure LogMuted
    formatP =
      option
        ( maybeReader $
            \case
              "plain" -> Just Plain
              "json" -> Just Json
              _ -> Nothing
        )
        ( long "format"
            <> value Plain
            <> showDefaultWith (map Char.toLower . show)
            <> metavar "FORMAT"
            <> help "Output format (plain|json)"
        )
    targetP =
      (,)
        <$> option
          (RemoteBaseURL <$> str)
          ( long "remote-base-url"
              <> short 'u'
              <> metavar "URL"
              <> help "Remote base URL (e.g. https://example.org/ping/)"
          )
        <*> option
          (Folder <$> str)
          ( long "directory"
              <> short 'd'
              <> metavar "DIRECTORY_PATH"
              <> help "Directory to the HTML (e.g. build/public)"
          )

-- * Meta

hopHeader :: String
hopHeader = "croque-mort - Dead simple broken links checker on local HTML folders"

hopDesc :: String
hopDesc = "A CLI to check broken links in HTML folders"
