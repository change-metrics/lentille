{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Main (main) where

import Lentille.GRPC.Api (run)
import Relude

main :: IO ()
main = run 8042
