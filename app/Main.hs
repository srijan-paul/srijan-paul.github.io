{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bark.CLI (BarkCLI (BarkCLI), builtinProcessors, doCommand, parseCommand)
import Bark.Core (urlFromMdPath)
import Bark.FrontMatter (PostFrontMatter (..))
import Bark.Types
  ( ErrorMessage,
    Post (..),
    Preprocessor,
    Processor (..),
    Project,
    Value (..),
    fromList,
  )
import Control.Monad ((>=>))
import Control.Monad.Except (liftEither)
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Internal.Date (formatDateMonDD, getYear, parseDate)
import System.Environment (getArgs)
import TextShow (TextShow (showt))

getDate :: Post -> Maybe T.Text
getDate post = do
  let fm = postFrontMatter post
  dateField <- HM.lookup "date" (fmMetaData fm)
  case dateField of
    String s -> Just s
    _ -> Nothing

-- | Add a `formattedDate` field to the post.
-- It is a string in the format `Mon DD`.
-- Example: "Jan 01"
addDateToPost :: Preprocessor
addDateToPost _ _ post = do
  let date = getDate post >>= parseDate
      postData = postOtherData post
  return $ case date of
    Just d ->
      let formattedDate = formatDateMonDD d
       in post {postOtherData = ("formattedDate", String formattedDate) : postData}
    Nothing -> post

getTemplate :: Post -> Maybe T.Text
getTemplate post = do
  let fm = postFrontMatter post
  templateField <- HM.lookup "template" (fmMetaData fm)
  case templateField of
    String s -> Just s
    _ -> Nothing

populateBlogHome :: Preprocessor
populateBlogHome project allPosts post = do
  let template = fromMaybe "" (getTemplate post)
  if template == "blog-home"
    then liftEither $ addAllBlogs project allPosts post
    else return post

addAllBlogs :: Project -> [Post] -> Post -> Either ErrorMessage Post
addAllBlogs project posts post = do
  allBlogs <- getBlogPosts posts
  return $ post {postOtherData = ("blogs", allBlogs) : postOtherData post}
  where
    getBlogPosts :: [Post] -> Either ErrorMessage Value
    getBlogPosts allPosts = do
      let blogPosts = filter isBlogPost allPosts
          maybeYears = nub <$> mapM (getDate >=> getYear) blogPosts
       in case maybeYears of
            Nothing -> Left "All blog posts must contain a date field in YYYY-MM-DD format."
            Just years -> do
              let postsByYear = sortOn (Down . fst) $ fmap (\y -> (y, postsInYear y blogPosts)) years
                  grouped = fromList $ fmap toObject postsByYear
                  toObject (y, blogs) = Object $ HM.fromList [("year", String $ showt y), ("posts", blogs)]
               in Right grouped

    sortByDate :: [Post] -> [Post]
    sortByDate = sortOn (Down . (getDate >=> parseDate))

    postsInYear :: Int -> [Post] -> Value
    postsInYear year blogPosts =
      let blogPostsInYear = sortByDate $ filter isInYear blogPosts
       in fromList $ fmap postToValue blogPostsInYear
      where
        isInYear p = case getDate p >>= getYear of
          Just y -> y == year
          Nothing -> False

    postToValue :: Post -> Value
    postToValue p =
      let metaData =
            [ ("meta", Object $ fmMetaData $ postFrontMatter p),
              ("url", String $ T.pack $ urlFromMdPath project $ postPath p)
            ]
          otherData = postOtherData p
       in Object $ HM.fromList (metaData ++ otherData)

    isBlogPost p =
      case HM.lookup "is_blog_post" (fmMetaData $ postFrontMatter p) of
        Just (Bool b) -> b
        _ -> False

main :: IO ()
main = do
  let processors = builtinProcessors ++ [OnPost addDateToPost, OnPost populateBlogHome]
      cli = BarkCLI processors
  maybeCommand <- parseCommand <$> getArgs
  case maybeCommand of
    Just command -> doCommand cli command
    Nothing -> putStrLn "Invalid command"
