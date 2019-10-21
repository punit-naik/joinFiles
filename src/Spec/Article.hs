{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Spec.Article where

import GHC.Generics
import Data.Aeson

data Article = Article {id :: Int
                        , author_id :: Int
                        , title :: String
                        , created_at :: String
                        , rating :: Int} deriving (Show, Generic, FromJSON, ToJSON)