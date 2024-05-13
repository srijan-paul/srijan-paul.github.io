{-# LANGUAGE OverloadedStrings #-}

module Main where

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
    foldM,
    fromList,
    toList,
  )
import Control.Monad ((>=>))
import Control.Monad.Except (liftEither)
import Control.Monad.State (MonadState (get, put))
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Internal.Date (Date (dateYear), formatDateMonDD, getYear, parseDate)
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
  put $ compilation {compilationPages = compilationPages compilation ++ tagPages}
  where
    tagToHtmlPage :: T.Text -> Project -> [Post] -> ExceptT ErrorMessage IO HTMLPage
    tagToHtmlPage tag project posts = do
      let taggedPosts = postsByTag tag posts
          templatePath = projectTemplateDir project </> "tags.mustache"
          content = wrapInHtml taggedPosts
          compileData = Object $ HM.fromList [("tag", String tag), ("content", content)]
      compiledHtml <- template2Html templatePath compileData
      let dstPath = projectOutDir project </> "tags" </> (T.unpack tag <> ".html")
      return $
        HTMLPage
          { htmlPagePost = Nothing,
            htmlPagePath = dstPath,
            htmlPageContent = compiledHtml
          }

    postsByTag :: T.Text -> [Post] -> [Post]
    postsByTag tag = filter (\p -> tag `elem` getTags (postFrontMatter p))

    wrapInHtml :: [Post] -> Value
    wrapInHtml posts = do
      let titleUrlPairs = mapMaybe pairPost posts
          listItems = T.concat $ map li titleUrlPairs
       in String $ "<ul class=\"post-list\">" <> listItems <> "</ul>"
      where
        pairPost :: Post -> Maybe (T.Text, T.Text, T.Text)
        pairPost post = case (postTitle post, postDate post) of
          (Just title, Just date) -> Just (T.pack $ postUrl post, title, date)
          _ -> Nothing

        postDate :: Post -> Maybe T.Text
        postDate post =
          getDate post >>= parseDate >>= Just . showt . dateYear

        li :: (T.Text, T.Text, T.Text) -> T.Text
        li (url, text, date) =
          "<li><a href=\"/"
            <> url
            <> "\">"
            <> text
            <> "</a>"
            <> " <span class=\"post-list-date\">"
            <> date
            <> "</span>"
            <> "</li>\n"

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

main :: IO ()
main = do
  let processors = builtinPlugins ++ [addDateToPosts, populateBlogHome, generateTagPages]
      cli = BarkCLI processors
  maybeCommand <- parseCommand <$> getArgs
  case maybeCommand of
    Just command -> doCommand cli command
    Nothing -> putStrLn "Invalid command"
