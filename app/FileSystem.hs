{-# LANGUAGE TupleSections #-}

module FileSystem
  ( parseLinks,

    -- * Tests
    pureParseLinks,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Poolboy (defaultPoolboySettings)
import Data.Poolboy.Tactics (concurrentRecursive')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Domain
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.HTML.Scalpel

parseLinks :: [(RemoteBaseURL, Folder)] -> IO ParsedLinks
parseLinks targets = do
  let go ::
        (RemoteBaseURL, Folder, FilePath) ->
        IO ([(RemoteBaseURL, Folder, FilePath)], [(RemoteBaseURL, WithRootFolder (Map.Map FilePath [Link]))])
      go (remoteBase, root, path) = do
        isFile <- doesFileExist path
        if isFile
          then do
            links <-
              if takeExtension path == ".html"
                then pureParseLinks <$> T.readFile path
                else return []
            return ([], [(remoteBase, WithRootFolder root $ Map.singleton path links)])
          else do
            entries <- listDirectory path
            return ((remoteBase,root,) . (path </>) <$> entries, [])
  ParsedLinks . Map.fromListWith (\x y -> WithRootFolder x.root $ x.links <> y.links) . concat
    <$> concurrentRecursive'
      defaultPoolboySettings
      (map go)
      ((map $ \(remoteBase, root) -> go (remoteBase, root, root.unFolder)) targets)

pureParseLinks :: T.Text -> [Link]
pureParseLinks input = fromMaybe [] $ scrapeStringLike input getLinks
  where
    getLinks :: Scraper T.Text [Link]
    getLinks = do
      meta <- chroot "head" $ attrs "href" "link"
      body <- chroot "body" $ (<>) <$> attrs "href" "a" <*> attrs "img" "src"
      script <- attrs "src" "script"
      return $ concatMap (mapMaybe parseLink) [meta, body, script]
