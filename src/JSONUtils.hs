{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields, DeriveAnyClass #-}

module JSONUtils where

import GHC.Generics
import Data.Aeson
import Data.Maybe as DMaybe
import Data.ByteString.Lazy as BS

import Spec.Author (Author)
import Spec.Article (Article)

parseAuthorsJSON :: BS.ByteString -> [Author]
parseAuthorsJSON x = Prelude.concat . DMaybe.maybeToList $ decode x

parseArticlesJSON :: BS.ByteString -> [Article]
parseArticlesJSON x = Prelude.concat . DMaybe.maybeToList $ decode x