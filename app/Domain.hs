{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoFieldSelectors #-}

module Domain
  ( Folder (..),
    RemoteBaseURL (..),
    Link (..),
    originalLink,
    parseLink,
    ParsedLinks (..),
    WithRootFolder (..),
    LinkReport (..),
    LinkStatus (..),
    CheckedLinks (..),
    checkLinks,
    BrokenLinks (..),
    filterBrokenLinks,
    hasBrokenLinks,
    Summary (..),
    SummaryPhase (..),
    mkSummary,
  )
where

import Data.Aeson
import qualified Data.Char as Char
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import GHC.Generics
import HTMLEntities.Decoder (htmlEncodedText)
import qualified System.FilePath as Path

newtype Folder
  = Folder {unFolder :: FilePath}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, ToJSONKey)

newtype RemoteBaseURL
  = RemoteBaseURL {unRemoteBaseURL :: T.Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, ToJSONKey)

data Link
  = LocalLink T.Text FilePath
  | RemoteLink T.Text T.Text
  deriving stock (Eq, Ord, Show, Generic)

originalLink :: Link -> T.Text
originalLink =
  \case
    LocalLink o _ -> o
    RemoteLink o _ -> o

linkOptions :: Options
linkOptions =
  defaultOptions
    { sumEncoding =
        TaggedObject
          { tagFieldName = "type",
            contentsFieldName = "link"
          }
    }

instance ToJSON Link where
  toJSON = genericToJSON linkOptions
  toEncoding = genericToEncoding linkOptions

parseLink :: T.Text -> Maybe Link
parseLink raw
  | "#" `T.isPrefixOf` raw || T.null (T.strip raw) = Nothing
  | "javascript:" `T.isPrefixOf` raw || "data:" `T.isPrefixOf` raw = Nothing
  | "https://" `T.isPrefixOf` clean || "http://" `T.isPrefixOf` clean = Just $ RemoteLink raw clean
  | otherwise = Just $ LocalLink raw $ T.unpack clean
  where
    clean = T.takeWhile (`notElem` ['#', '?']) $ TL.toStrict $ TL.toLazyText $ htmlEncodedText raw

newtype ParsedLinks
  = ParsedLinks {unParsedLinks :: Map.Map RemoteBaseURL (WithRootFolder (Map.Map FilePath [Link]))}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON)

data WithRootFolder a = WithRootFolder
  { root :: Folder,
    links :: a
  }
  deriving stock (Eq, Ord, Show, Generic, Functor)
  deriving anyclass (ToJSON)

data LinkReport = LinkReport
  { link :: Link,
    status :: LinkStatus
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data LinkStatus
  = Skipped
  | Valid
  | Broken
  deriving stock (Eq, Ord, Show, Generic)

instance Semigroup LinkStatus where
  Skipped <> x = x
  x <> Skipped = x
  _ <> Valid = Valid
  Valid <> _ = Valid
  Broken <> Broken = Broken

linkStatusOptions :: Options
linkStatusOptions =
  defaultOptions
    { constructorTagModifier = map Char.toLower
    }

instance ToJSON LinkStatus where
  toJSON = genericToJSON linkStatusOptions
  toEncoding = genericToEncoding linkStatusOptions

newtype CheckedLinks
  = CheckedLinks {unCheckedLinks :: Map.Map RemoteBaseURL (WithRootFolder (Map.Map FilePath [LinkReport]))}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON)

checkLinks :: ParsedLinks -> CheckedLinks
checkLinks (ParsedLinks links) =
  CheckedLinks $
    Map.map
      ( \withRoot ->
          WithRootFolder
            { root = withRoot.root,
              links = Map.filter (not . null) $ Map.mapWithKey (map . checkLink withRoot.root) withRoot.links
            }
      )
      links
  where
    checkLink (Folder root) file link =
      LinkReport
        { link = link,
          status =
            let fixIndex targetFile
                  | null targetFile = "index.html"
                  | "/" `isSuffixOf` targetFile = targetFile <> "index.html"
                  | otherwise = targetFile
             in case link of
                  LocalLink _ localLink ->
                    let targetFile = fixIndex localLink
                        fullFile =
                          Path.normalise $
                            if "/" `isPrefixOf` targetFile
                              then root Path.</> ("." <> targetFile)
                              else Path.takeDirectory file Path.</> targetFile
                     in if any (Map.member fullFile . (.links)) $ Map.elems links
                          then Valid
                          else Broken
                  RemoteLink _ remoteLink ->
                    let checkRemote (RemoteBaseURL remoteBase) withRoot =
                          flip fmap (T.stripPrefix remoteBase remoteLink) $ \subFile ->
                            let targetFile = fixIndex $ T.unpack $ fromMaybe subFile $ T.stripPrefix "/" subFile
                             in if Map.member (Path.normalise $ withRoot.root.unFolder Path.</> targetFile) withRoot.links
                                  then Valid
                                  else Broken
                     in fromMaybe Skipped $
                          Map.foldlWithKey'
                            (\acc remoteBase withRoot -> acc <> checkRemote remoteBase withRoot)
                            Nothing
                            links
        }

newtype BrokenLinks
  = BrokenLinks {unBrokenLinks :: Map.Map RemoteBaseURL (WithRootFolder (Map.Map FilePath [Link]))}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON)

filterBrokenLinks :: CheckedLinks -> BrokenLinks
filterBrokenLinks (CheckedLinks checked) =
  BrokenLinks $
    Map.filter (not . null . (.links)) $
      Map.map (fmap $ Map.filter (not . null) . Map.map (map (.link) . filter ((== Broken) . (.status)))) checked

hasBrokenLinks :: BrokenLinks -> Bool
hasBrokenLinks (BrokenLinks broken) = not $ Map.null broken

data Summary = Summary
  { parsing :: SummaryPhase,
    checking :: SummaryPhase,
    broken :: SummaryPhase
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data SummaryPhase = SummaryPhase
  { remoteBaseURLs :: Int,
    files :: Int,
    links :: Int,
    linksSkipped :: Maybe Int,
    linksValid :: Maybe Int,
    linksBroken :: Maybe Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

mkSummary :: ParsedLinks -> CheckedLinks -> BrokenLinks -> Summary
mkSummary (ParsedLinks parsed) (CheckedLinks checked) (BrokenLinks broken) =
  Summary
    { parsing =
        phase parsed $ const (Nothing, Nothing, Nothing),
      checking =
        phase checked $ \allLinks ->
          ( Just $ length $ filter ((== Skipped) . (.status)) allLinks,
            Just $ length $ filter ((== Valid) . (.status)) allLinks,
            Just $ length $ filter ((== Broken) . (.status)) allLinks
          ),
      broken =
        phase broken $ \allLinks -> (Nothing, Nothing, Just $ length allLinks)
    }
  where
    phase ::
      Map.Map RemoteBaseURL (WithRootFolder (Map.Map FilePath [a])) ->
      ([a] -> (Maybe Int, Maybe Int, Maybe Int)) ->
      SummaryPhase
    phase nestedMap getStats =
      let allFiles = map (.links) $ Map.elems nestedMap
          allLinks = concat $ concatMap Map.elems allFiles
          (skippedCount, validCount, brokenCount) = getStats allLinks
       in SummaryPhase
            { remoteBaseURLs = Map.size nestedMap,
              files = sum $ map Map.size allFiles,
              links = length allLinks,
              linksSkipped = skippedCount,
              linksValid = validCount,
              linksBroken = brokenCount
            }
