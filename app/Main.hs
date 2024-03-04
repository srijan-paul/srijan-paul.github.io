{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bark.CLI (BarkCLI (BarkCLI), builtinProcessors, doCommand, parseCommand)
import Bark.FrontMatter (PostFrontMatter (..))
import Bark.Types
  ( Post (..),
    Preprocessor,
    Processor (..),
    Value (..),
  )
import qualified Data.HashMap.Strict as HM
import System.Environment (getArgs)
import Internal.Date (formatDateMonDD, parseDate)

-- | Add a `formattedDate` field to the post.
-- It is a string in the format `Mon DD`.
-- Example: "Jan 01"
addDateToPost :: Preprocessor
addDateToPost _project post = do
  let (postData, frontmatter) = (postOtherData post, postFrontMatter post)
      date = HM.lookup "date" (fmMetaData frontmatter) >>= parseDate
  return $ case date of
    Just d ->
      let formattedDate = formatDateMonDD d
       in post {postOtherData = ("formattedDate", String formattedDate) : postData }
    Nothing -> post

main :: IO ()
main = do
  let processors = OnPost addDateToPost : builtinProcessors
      cli = BarkCLI processors
  maybeCommand <- parseCommand <$> getArgs
  case maybeCommand of
    Just command -> doCommand cli command
    Nothing -> putStrLn "Invalid command"

