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
import qualified Data.Text as T
import Network.URI (URI (..), URIAuth (uriPort, uriRegName), nullURI, nullURIAuth)
import qualified Text.RSS as RSS

isBlogPost :: Post -> Bool
isBlogPost post = case HM.lookup "is_blog_post" metaData of
  Just (Bool b) -> b
  _ -> False
  where
    metaData = fmMetaData $ postFrontMatter post

postToRssItem :: Post -> RSS.Item
postToRssItem post =
  [ RSS.Title title,
    RSS.Description description,
    RSS.Author author,
    RSS.Link link
  ]
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
        { uriScheme = "https",
          uriAuthority = Just (nullURIAuth {uriRegName = "injuly.in", uriPort = ":443"}),
          uriPath = "/blog" ++ '/' : relPath
        }

    relPath :: FilePath
    relPath = dropExtension (takeBaseName (postPath post)) <> "/index.html"

rssChannel :: [RSS.ChannelElem]
rssChannel =
  [ RSS.Language "en-us",
    RSS.Copyright "Srijan Paul",
    RSS.Generator "Bark"
  ]

makeFeed :: [RSS.Item] -> RSS.RSS
makeFeed =
  RSS.RSS
    ""
    ( nullURI
        { uriScheme = "https",
          uriAuthority = Just (nullURIAuth {uriRegName = "injuly.in", uriPort = ":443"}),
          uriPath = "/blog"
        }
    )
    ""
    rssChannel

-- plugin to add an RSS feed to the site
addRssFeed :: Plugin
addRssFeed = AfterBuild $ do
  compilation <- get
  let blogPosts = filter isBlogPost (compilationPosts compilation)
      project = compilationProject compilation

  let rss = makeFeed $ postToRssItem <$> blogPosts
      -- TODO: can I use Text here instead?
      -- Does GHC optimize this because of `T.pack`
      xmlString = T.pack <$> RSS.showXML $ RSS.rssToXML rss
      page =
        HTMLPage
          { htmlPageContent = xmlString,
            htmlPagePath = projectOutDir project </> "rss.xml",
            htmlPagePost = Nothing
          }
  put $ compilation {compilationPages = page : compilationPages compilation}
