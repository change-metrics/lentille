{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- TMP
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lentille.GitLab where

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Data.Time.Clock
import Lentille (LentilleError (DecodeError), LentilleStream, stopLentille)
import Monocle.Api.Client.Worker (mkManager)
import qualified Network.HTTP.Client as HTTP
import qualified Network.URI as URI
import Relude
import qualified Streaming.Prelude as S

schemaLocation :: String
schemaLocation = "./gitlab-schema/schema.graphql"

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GitLabGraphClient = GitLabGraphClient
  { manager :: HTTP.Manager,
    url :: Text,
    token :: Text,
    host :: Text
  }

newGitLabGraphClientWithKey :: MonadIO m => Text -> Text -> m GitLabGraphClient
newGitLabGraphClientWithKey url' token' = do
  manager' <- mkManager
  let host' =
        maybe
          (error "Unable to parse provided gitlab_url")
          (toText . URI.uriRegName)
          (URI.uriAuthority =<< URI.parseURI (toString url'))
  pure $ GitLabGraphClient manager' url' token' host'

newGitLabGraphClient :: MonadIO m => Text -> m GitLabGraphClient
newGitLabGraphClient url' = do
  token' <-
    toText
      . fromMaybe (error "GITLAB_GRAPH_TOKEN environment is missing")
      <$> liftIO (lookupEnv "GITLAB_GRAPH_TOKEN")
  newGitLabGraphClientWithKey url' token'

runGitLabGraphRequest :: MonadIO m => GitLabGraphClient -> LBS.ByteString -> m LBS.ByteString
runGitLabGraphRequest (GitLabGraphClient manager' url' token' _) jsonBody = do
  -- putTextLn $ "Sending this query: " <> decodeUtf8 jsonBody
  let initRequest = HTTP.parseRequest_ (toString url')
      request =
        initRequest
          { HTTP.method = "POST",
            HTTP.requestHeaders =
              [ ("Authorization", "Bearer " <> encodeUtf8 token'),
                ("User-Agent", "change-metrics/lentille-morpheus"),
                ("Content-Type", "application/json")
              ],
            HTTP.requestBody = HTTP.RequestBodyLBS jsonBody
          }
  response <- liftIO $ HTTP.httpLbs request manager'
  -- print response
  pure (HTTP.responseBody response)

data PageInfo = PageInfo {hasNextPage :: Bool, endCursor :: Maybe Text, totalCount :: Maybe Int}
  deriving (Show)

streamFetch ::
  (Fetch a, FromJSON a) =>
  GitLabGraphClient ->
  -- | MR updatedAt date until we need to fetch
  Maybe UTCTime ->
  -- | query Args constructor, the function takes a cursor
  (Text -> Args a) ->
  -- | query result adapter
  (a -> (PageInfo, [Text], [b])) ->
  -- | check for limit ->
  (LentilleStream b -> LentilleStream b) ->
  LentilleStream b
streamFetch client untilDate mkArgs transformResponse checkLimit = checkLimit $ go Nothing
  where
    logStatus (PageInfo hasNextPage' _ totalCount') =
      putTextLn $
        "[gitlab-graphql] got total count of documents: "
          <> show totalCount'
          <> " fetching until date: "
          <> show untilDate
          <> (if hasNextPage' then " [hasNextPage] " else "")
    go pageInfoM = do
      -- Get current page results
      respE <-
        fetch
          (runGitLabGraphRequest client)
          (mkArgs . fromMaybe (error "Missing endCursor") $ maybe (Just "") endCursor pageInfoM)
      let (pageInfo, decodingErrors, xs) = case respE of
            Left err -> error (toText err)
            Right resp -> transformResponse resp

      logStatus pageInfo

      -- Yield the results
      S.each xs

      -- Abort the stream when there are errors
      unless (null decodingErrors) (stopLentille $ DecodeError decodingErrors)

      -- TODO: implement throttle
      when (hasNextPage pageInfo) (go (Just pageInfo))
