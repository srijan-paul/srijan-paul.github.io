{-# LANGUAGE OverloadedStrings #-}

module Internal.Rss (addRssFeed) where

import Bark.Core (dropExtension, takeBaseName, (</>))
import Bark.FrontMatter (PostFrontMatter (..))
import Bark.Types
  ( Compilation (..),
    HTMLPage (..),
    MonadState (get, put),
    Plugin (..),
    Post (..),
    Project (..),
    Value (..),
  )
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Internal.Date (Date (..), parseDate)
import Network.URI (URI (..), URIAuth (uriRegName), nullURI, nullURIAuth)
import qualified Text.RSS as RSS

isBlogPost :: Post -> Bool
isBlogPost post = case HM.lookup "is_blog_post" metaData of
  Just (Bool b) -> b
  _ -> False
  where
    metaData = fmMetaData $ postFrontMatter post

monthToInt :: Date -> Int
monthToInt date = fromEnum (dateMonth date) + 1

dateToUTCTime :: Date -> UTCTime
dateToUTCTime date =
  UTCTime
    (fromGregorian (fromIntegral $ dateYear date) (monthToInt date) (dateDay date))
    (secondsToDiffTime 0)

getPostDate :: Post -> Maybe Date
getPostDate post = case HM.lookup "date" (fmMetaData $ postFrontMatter post) of
  Just (String s) -> parseDate s
  _ -> Nothing

postToRssItem :: Post -> RSS.Item
postToRssItem post =
  [ RSS.Title title,
    RSS.Description description,
    RSS.Author author,
    RSS.Link link,
    RSS.Guid True (show link)
  ]
    ++ pubDateElem
  where
    metaData = fmMetaData $ postFrontMatter post

    title :: String
    title = case HM.lookup "title" metaData of
      Just (String s) -> T.unpack s
      _ -> "No title"

    description :: String
    description = case HM.lookup "meta" metaData of
      Just (String s) -> T.unpack s
      _ -> "No description"

    author :: String
    author = "srijan@injuly.in"

    link :: URI
    link =
      nullURI
        { uriScheme = "https:",
          uriAuthority = Just (nullURIAuth {uriRegName = "injuly.in"}),
          uriPath = "/blog" ++ '/' : relPath
        }

    relPath :: FilePath
    relPath = dropExtension (takeBaseName (postPath post)) <> "/index.html"

    pubDateElem :: [RSS.ItemElem]
    pubDateElem = case getPostDate post of
      Just date -> [RSS.PubDate (dateToUTCTime date)]
      Nothing -> []

rssChannel :: [RSS.ChannelElem]
rssChannel =
  [ RSS.Language "en-us",
    RSS.Copyright "Srijan Paul",
    RSS.Generator "Bark"
  ]

makeFeed :: [RSS.Item] -> RSS.RSS
makeFeed =
  RSS.RSS
    "injuly.in"
    ( nullURI
        { uriScheme = "https:",
          uriAuthority = Just (nullURIAuth {uriRegName = "injuly.in"}),
          uriPath = "/blog"
        }
    )
    "Srijan Paul's blog"
    rssChannel

-- plugin to add an RSS feed to the site
addRssFeed :: Plugin
addRssFeed = AfterBuild $ do
  compilation <- get
  let blogPosts = filter isBlogPost (compilationPosts compilation)
      project = compilationProject compilation
      sortedPosts = sortOn (Down . getPostDate) blogPosts

  let rss = makeFeed $ postToRssItem <$> sortedPosts
      xmlString = T.pack <$> RSS.showXML $ RSS.rssToXML rss
      page =
        HTMLPage
          { htmlPageContent = xmlString,
            htmlPagePath = projectOutDir project </> "rss.xml",
            htmlPagePost = Nothing
          }
  put $ compilation {compilationPages = page : compilationPages compilation}
