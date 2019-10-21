{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields, DeriveAnyClass #-}

module Lib where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.List as DList
import Data.Maybe as DMaybe
import Data.Map as DMap

import Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import qualified System.Environment   as IO

import JSONUtils as JU
import Spec.Author as AuthSpec
import Spec.Article as ArtSpec
import Spec.AuthorWithArticles as AuthArtSpec

comparingAuthorId (ArtSpec.Article _ a1 _ _ _) (ArtSpec.Article _ a2 _ _ _)
    | a1 < a2 = LT
    | a1 > a2 = GT
    | a1 == a2 = GT

groupByAuthorId :: [ArtSpec.Article] -> Map Int [ArtSpec.Article]
groupByAuthorId x = DMap.fromList
                    (DList.map (\x -> ((author_id (head x)), x))
                      (groupBy (\x y -> (author_id x) == (author_id y))
                        (sortBy comparingAuthorId x)))

justLookupArticles :: Map Int [ArtSpec.Article] -> Int -> [ArtSpec.Article]
justLookupArticles m k = DMaybe.fromMaybe [] (DMap.lookup k m)

joinAuthorsAndArticles :: [AuthSpec.Author] -> Map Int [ArtSpec.Article] -> [String] -> [AuthArtSpec.AuthorWithArticles]
joinAuthorsAndArticles authors groupedArticles filters = do
                                  DList.map (\x -> do
                                    let articlez = (justLookupArticles groupedArticles (AuthSpec.id x))
                                    let filteredArticles = DList.filter (\x -> do
                                                                          (rating x) > (if (DList.elem "filter=ratingGreater3" filters) then 3 else 0))
                                                            (DList.take (if (DList.elem "filter=first5" filters)
                                                                          then 5
                                                                          else (DList.length articlez))
                                                              articlez)
                                    AuthArtSpec.AuthorWithArticles
                                      (AuthSpec.id x) (AuthSpec.name x) filteredArticles) authors

joinFiles :: String -> String -> [String] -> IO ()
joinFiles authors articles filters = do
  authorsFile <- BS.readFile (authors)
  articlesFile <- BS.readFile (articles)
  let authors = JU.parseAuthorsJSON authorsFile
  let groupedArticles = groupByAuthorId (JU.parseArticlesJSON articlesFile)
  let authorsWithArticles = joinAuthorsAndArticles authors groupedArticles filters
  let d = encodeToLazyText authorsWithArticles
  I.writeFile "authors_with_articles.json" d