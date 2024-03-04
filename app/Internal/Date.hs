{-# LANGUAGE OverloadedStrings #-}

module Internal.Date where


import Text.Read (readMaybe)
import TextShow
import Bark.Types (Value(..), Preprocessor)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data Month
  = Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Show, Eq, Enum)

data Date = Date
  { dateYear :: Int,
    dateMonth :: Month,
    dateDay :: Int
  } deriving (Show, Eq)

formatDateMonDD :: Date -> T.Text
formatDateMonDD date = T.pack (show (dateMonth date)) <> " " <> showt (dateDay date)

read' :: Read a => String -> Maybe a
read' = readMaybe

stringifyDate :: Date -> T.Text
stringifyDate date =
  showt (dateYear date)
    <> "-"
    <> T.pack (show (dateMonth date))
    <> "-"
    <> showt (dateDay date)

parseYear :: String -> Maybe Int
parseYear = readMaybe

parseMonth :: String -> Maybe Month
parseMonth s = case read' s of
  Just m 
    | m >= 1 && m <= 12 -> Just $ toEnum (m - 1) -- 0-indexed
    | otherwise -> Nothing
  _ -> Nothing


parseDay :: Month -> String -> Maybe Int
parseDay month s = read' s >>= validate
  where
    validate :: Int -> Maybe Int
    validate d
      | d <= daysInMonth month = Just d
      | otherwise = Nothing

    daysInMonth :: Month -> Int
    daysInMonth m = case m of
      Feb -> 29
      Apr -> 30
      Jun -> 30
      Sep -> 30
      Nov -> 30
      _ -> 31

parseDateS :: T.Text -> Maybe Date
parseDateS = parse . T.splitOn "-"
  where
    parse :: [T.Text] -> Maybe Date
    parse [y, m, d] = do
      year <- parseYear (T.unpack y)
      month <- parseMonth (T.unpack m)
      day <- parseDay month (T.unpack d)
      return $ Date {dateYear = year, dateMonth = month, dateDay = day}
    parse _ = Nothing

parseDate :: Value -> Maybe Date
parseDate (String s) = parseDateS s
parseDate _ = Nothing


