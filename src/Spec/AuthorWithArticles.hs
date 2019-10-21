{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Spec.AuthorWithArticles where

import GHC.Generics
import Data.Aeson

import Spec.Article (Article)

data AuthorWithArticles = AuthorWithArticles {id :: Int, name :: String, articles :: [Article]} deriving (Show, Generic, FromJSON, ToJSON)