{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Macroscope.Main (runMacroscope) where

import Control.Concurrent (threadDelay)
import Lentille.GitLab (GitLabGraphClient, newGitLabGraphClientWithKey)
import Lentille.GitLab.MergeRequests (streamMergeRequests)
import Macroscope.Worker (DocumentStream (..), runStream)
import Monocle.Api.Client
import qualified Monocle.Api.Config as Config
import Monocle.Prelude
import qualified Network.URI as URI

-- | 'MacroM' is an alias for a bunch of constrain.
class (MonadIO m, MonadFail m, MonadMask m, MonadLog m) => MacroM m

instance MacroM IO

-- | Utility function to create a flat list of crawler from the whole configuration
getCrawlers :: [Config.Index] -> [(Text, Text, Config.Crawler)]
getCrawlers xs = do
  index <- xs
  crawler <- fromMaybe [] (Config.crawlers index)
  let name = Config.index index
      key = fromMaybe (error "Api key is missing") (Config.crawlers_api_key index)
  pure (name, key, crawler)

crawlerName :: Config.Crawler -> Text
crawlerName Config.Crawler {..} = name

-- | 'run' is the entrypoint of the macroscope process
runMacroscope :: MacroM m => Bool -> FilePath -> Word32 -> MonocleClient -> m ()
runMacroscope verbose confPath interval client = do
  monocleLog "Macroscope begin..."
  confE <- Config.loadConfig confPath
  case confE of
    Left err -> error ("Unable to read the configuration file: " <> err)
    Right conf -> loop conf
  where
    loop conf = do
      -- Crawl each index
      traverse_ crawl (getCrawlers conf)

      -- Pause
      liftIO $ threadDelay interval_usec

      -- Loop again
      loop conf

    interval_usec = fromInteger . toInteger $ interval * 1_000_000

    crawl :: MacroM m => (Text, Text, Config.Crawler) -> m ()
    crawl (index, key, crawler) = do
      now <- liftIO getCurrentTime
      when verbose (monocleLog $ "Crawling " <> crawlerName crawler)

      -- Create document streams
      docStreams <- case Config.provider crawler of
        Config.GitlabProvider Config.Gitlab {..} -> do
          -- TODO: the client may be created once for each api key
          glClient <- newGitLabGraphClientWithKey gitlab_url gitlab_api_key
          let host =
                maybe
                  (error "Unable to parse provided gitlab_url")
                  (toText . URI.uriRegName)
                  (URI.uriAuthority =<< URI.parseURI (toString gitlab_url))
          pure $
            -- When organizations are configured, we need to index its project first
            [glOrgCrawler glClient | isJust gitlab_organizations]
              -- Then we always index the projects
              <> [glMRCrawler glClient host]
        _ -> error "NotImplemented"

      -- Consume each stream
      let runner =
            runStream client now (toLazy key) (toLazy index) (toLazy $ crawlerName crawler)
      -- TODO: handle exceptions
      traverse_ runner docStreams

    glMRCrawler :: MonadIO m => GitLabGraphClient -> Text -> DocumentStream m
    glMRCrawler glClient host = Changes $ streamMergeRequests glClient host

    glOrgCrawler :: GitLabGraphClient -> DocumentStream m
    glOrgCrawler _glClient = Projects $ error "Org NotImplemented"
