{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bark.CLI (BarkCLI (BarkCLI), builtinPlugins, doCommand, parseCommand)
import Bark.Core (template2Html, (</>))
import Bark.FrontMatter (PostFrontMatter (..))
import Bark.Types
  ( Compilation (..),
    ErrorMessage,
    ExceptT,
    HTMLPage (..),
    MonadTrans (lift),
    Plugin (..),
    Post (..),
    Project (..),
    Value (..),
    fromList,
    toList,
  )
import Control.Monad ((>=>), foldM)
import Control.Monad.Except (liftEither)
import Control.Monad.State (MonadState (get, put))
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Internal.Date (Date (dateYear), formatDateMonDD, getYear, parseDate)
import Internal.Rss (addRssFeed)
import System.Environment (getArgs)
import TextShow (TextShow (showt))

getTags :: PostFrontMatter -> [T.Text]
getTags fm =
  case HM.lookup "tags" (fmMetaData fm) of
    Just (Array tags) -> mapMaybe extractTag (toList tags)
    _ -> []
  where
    extractTag (String t) = Just t
    extractTag _ = Nothing

postTitle :: Post -> Maybe T.Text
postTitle = getTitle . postFrontMatter
  where
    getTitle fm =
      case HM.lookup "title" (fmMetaData fm) of
        Just (String t) -> Just t
        _ -> Nothing

generateTagPages :: Plugin
generateTagPages = AfterBuild $ do
  compilation <- get
  let posts = compilationPosts compilation
      tags = nub $ concatMap (getTags . postFrontMatter) posts
      project = compilationProject compilation

  let combine pages tag = do
        page <- lift $ tagToHtmlPage tag project posts
        return $ page : pages

  tagPages <- foldM combine [] tags
  put $
    compilation
      { compilationPages =
          compilationPages compilation ++ tagPages
      }
  where
    tagToHtmlPage :: T.Text -> Project -> [Post] -> ExceptT ErrorMessage IO HTMLPage
    tagToHtmlPage tag project posts = do
      let taggedPosts_ = filterPostsByTag tag posts
          taggedPosts = sortByDate taggedPosts_
          templatePath = projectTemplateDir project </> "tags.mustache"
          content = toHtmlUl taggedPosts
          compileData = Object $ HM.fromList [("tag", String tag), ("content", content)]
      compiledHtml <- template2Html templatePath compileData
      let fileName = T.toLower (T.replace " " "-" tag) <> ".html"
      let dstPath = projectOutDir project </> "tags" </> T.unpack fileName
      return $
        HTMLPage
          { htmlPagePost = Nothing,
            htmlPagePath = dstPath,
            htmlPageContent = compiledHtml
          }

    filterPostsByTag :: T.Text -> [Post] -> [Post]
    filterPostsByTag tag =
      filter
        (\p -> tag `elem` getTags (postFrontMatter p))

    sortByDate :: [Post] -> [Post]
    sortByDate = sortOn (Down . (getDate >=> parseDate))

    -- return an HTML rendered unordered list of posts.
    toHtmlUl :: [Post] -> Value
    toHtmlUl posts = do
      let linkPieces = mapMaybe postLinkPieces posts
          listItems = T.concat $ map li linkPieces
       in String $ "<ul class=\"post-list\">" <> listItems <> "</ul>"
      where
        --  returns a (URL, title, date) triple from a post.
        postLinkPieces :: Post -> Maybe (T.Text, T.Text, T.Text)
        postLinkPieces post = case (postTitle post, postYear post) of
          (Just title, Just date) -> Just (T.pack $ postUrl post, title, date)
          _ -> Nothing

        postYear :: Post -> Maybe T.Text
        postYear post = do
          dateStr <- getDate post
          date <- parseDate dateStr
          return $ showt (dateYear date)

        -- returns an HTML list item from a (URL, title, date) triple of a post.
        li :: (T.Text, T.Text, T.Text) -> T.Text
        li (url, text, date) =
          T.concat
            [ "<li><a href=\"/",
              url,
              "\">",
              text,
              "</a><span class=\"post-list-date\">",
              date,
              "</span>",
              "</li>\n"
            ]

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
addDateToPosts :: Plugin
addDateToPosts = BeforeBuild $ do
  compilation <- get
  let posts = compilationPosts compilation
      modifiedPosts = map addFormattedDate posts
  put $ compilation {compilationPosts = modifiedPosts}
  where
    addFormattedDate post =
      case getDate post >>= parseDate of
        Just date ->
          let formattedDate = formatDateMonDD date
              buildData = postData post
           in post {postData = HM.insert "formattedDate" (String formattedDate) buildData}
        Nothing -> post

getTemplate :: Post -> Maybe T.Text
getTemplate post = do
  let fm = postFrontMatter post
  templateField <- HM.lookup "template" (fmMetaData fm)
  case templateField of
    String s -> Just s
    _ -> Nothing

populateBlogHome :: Plugin
populateBlogHome = BeforeBuild $ do
  compilation <- get
  let posts = compilationPosts compilation
      newPosts' = mapM (f posts) posts

  case newPosts' of
    Left message -> liftEither $ Left message
    Right newPosts -> put $ compilation {compilationPosts = newPosts}
  where
    f :: [Post] -> Post -> Either ErrorMessage Post
    f allPosts currentPost =
      if isBlogHome currentPost
        then addAllBlogs allPosts currentPost
        else return currentPost

    isBlogHome post = getTemplate post == Just "blog-home"

addAllBlogs :: [Post] -> Post -> Either ErrorMessage Post
addAllBlogs posts post = do
  allBlogs <- getBlogPosts posts
  return $ post {postData = HM.insert "blogs" allBlogs (postData post)}
  where
    getBlogPosts :: [Post] -> Either ErrorMessage Value
    getBlogPosts allPosts = do
      let blogPosts = filter isBlogPost allPosts
          maybeYears = nub <$> mapM (getDate >=> getYear) blogPosts
       in case maybeYears of
            Nothing -> Left "A blog post is missing a 'date' field in YYYY-MM-DD format."
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
      let metaData = Object $ fmMetaData $ postFrontMatter p
          otherData = postData p
       in Object $ HM.insert "meta" metaData otherData

    isBlogPost p =
      case HM.lookup "is_blog_post" (fmMetaData $ postFrontMatter p) of
        Just (Bool b) -> b
        _ -> False

customPlugins :: [Plugin]
customPlugins =
  [ addDateToPosts, -- add a formatted date field to the post
    populateBlogHome, -- populate the blog home page with all blog posts
    generateTagPages, -- generate tag pages (e.g: injuly.in/tags/lua/index.html)
    addRssFeed -- add an RSS feed to the site
  ]

main :: IO ()
main = do
  let processors = builtinPlugins ++ customPlugins
      cli = BarkCLI processors
  maybeCommand <- parseCommand <$> getArgs
  maybe (return ()) (doCommand cli) maybeCommand
