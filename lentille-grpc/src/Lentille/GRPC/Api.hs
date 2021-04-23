{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Lentille.GRPC.Api (run) where

import qualified Monocle.Search as Monocle
import Network.GRPC.HighLevel.Generated
import Relude

handlers :: Monocle.Search ServerRequest ServerResponse
handlers = Monocle.Search {Monocle.searchGetSuggestions = getSuggestionHandler}

getSuggestionHandler ::
  ServerRequest 'Normal Monocle.GetSuggestionsRequest Monocle.GetSuggestionsResponse ->
  IO (ServerResponse 'Normal Monocle.GetSuggestionsResponse)
getSuggestionHandler (ServerNormalRequest _metadata (Monocle.GetSuggestionsRequest index)) = do
  putTextLn $ "Getting suggestions for index: " <> toStrict index
  let answer = Monocle.GetSuggestionsResponse ["FutureFeature", "Triaged"]
  pure (ServerNormalResponse answer [] StatusOk mempty)

options :: ServiceOptions
options = defaultServiceOptions

run :: Port -> IO ()
run port = do
  putTextLn $ "Serving 0.0.0.0:" <> show (unPort port)
  Monocle.searchServer handlers (options {serverPort = port})
