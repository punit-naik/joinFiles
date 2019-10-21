{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import JSONUtils (parseAuthorsJSON, parseArticlesJSON)
import Data.Map (fromList)
import Data.List (length)
import qualified Data.ByteString.Lazy as BS

import Spec.Author as AuthSpec
import Spec.Article as ArtSpec
import Spec.AuthorWithArticles as AuthArtSpec
import Lib (groupByAuthorId, justLookupArticles, joinAuthorsAndArticles)

parseAuthorsTest :: Assertion
parseAuthorsTest = do
                   let sampleData = "[{\"id\":1,\"name\":\"punit naik\"}]" :: BS.ByteString
                   let parsedSampleData = parseAuthorsJSON sampleData
                   ((length parsedSampleData) @?= 1)
                   ((AuthSpec.id (head parsedSampleData)) @?= 1)
                   ((AuthSpec.name (head parsedSampleData)) @?= "punit naik")

parseArticlesTest :: Assertion
parseArticlesTest = do
                    let sampleData = "[{\"id\":1,\"author_id\":1,\"title\":\"test\",\"created_at\":\"2019-01-01\",\"rating\":3}]" :: BS.ByteString
                    let parsedSampleData = parseArticlesJSON sampleData
                    ((length parsedSampleData) @?= 1)
                    ((ArtSpec.id (head parsedSampleData)) @?= 1)
                    ((ArtSpec.author_id (head parsedSampleData)) @?= 1)
                    ((ArtSpec.title (head parsedSampleData)) @?= "test")
                    ((ArtSpec.created_at (head parsedSampleData)) @?= "2019-01-01")
                    ((ArtSpec.rating (head parsedSampleData)) @?= 3)

groupArticlesByAuthorIdTest :: Assertion
groupArticlesByAuthorIdTest = do
                              let sampleArticle = (ArtSpec.Article 1 1 "test" "2019-01-01" 3)
                              let groupedArticles = groupByAuthorId [sampleArticle]
                              let fetchedArticle = head (justLookupArticles groupedArticles 1)
                              ((length groupedArticles) @?= 1)
                              ((ArtSpec.id fetchedArticle) @?= 1)
                              ((ArtSpec.author_id fetchedArticle) @?= 1)
                              ((ArtSpec.title fetchedArticle) @?= "test")
                              ((ArtSpec.created_at fetchedArticle) @?= "2019-01-01")
                              ((ArtSpec.rating fetchedArticle) @?= 3)

joinAuthorsAndArticlesTest :: Assertion
joinAuthorsAndArticlesTest = do
                             let sampleAuthor = (AuthSpec.Author 1 "punit naik")
                             let sampleArticleOne = (ArtSpec.Article 1 1 "test pass" "2019-01-01" 4)
                             let sampleArticleTwo = (ArtSpec.Article 2 1 "test fail" "2019-01-01" 2)
                             let groupedArticles = groupByAuthorId [sampleArticleOne, sampleArticleTwo]
                             let filters = ["filter=ratingGreater3"]
                             let joined = (joinAuthorsAndArticles [sampleAuthor] groupedArticles filters)
                             let fetchJoinedAuthor = head joined
                             ((length joined) @?= 1)
                             ((AuthArtSpec.id fetchJoinedAuthor) @?= 1)
                             ((AuthArtSpec.name fetchJoinedAuthor) @?= "punit naik")
                             ((ArtSpec.title (head (AuthArtSpec.articles fetchJoinedAuthor))) @?= "test pass")

main :: IO ()
main = defaultMainWithOpts
       [testCase "parseAuthorsTest" parseAuthorsTest
       , testCase "parseArticlesTest" parseArticlesTest
       , testCase "groupArticlesByAuthorIdTest" groupArticlesByAuthorIdTest
       , testCase "joinAuthorsAndArticlesTest" joinAuthorsAndArticlesTest]
       mempty