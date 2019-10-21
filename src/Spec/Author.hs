{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Spec.Author where

import GHC.Generics
import Data.Aeson

data Author = Author {id :: Int, name :: String} deriving (Show, Generic, FromJSON, ToJSON)