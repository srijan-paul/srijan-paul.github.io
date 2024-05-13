{-# LANGUAGE OverloadedStrings #-}

module Internal.Date where

import qualified Data.Text as T
import Text.Read (readMaybe)
import TextShow

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
  deriving (Show, Eq, Enum, Ord)

data Date = Date
  { dateYear :: Int,
    dateMonth :: Month,
    dateDay :: Int
  }
  deriving (Show, Eq)

instance Ord Date where
  compare a b
    | dateYear a /= dateYear b = compare (dateYear a) (dateYear b)
    | dateMonth a /= dateMonth b = compare (dateMonth a) (dateMonth b)
    | otherwise = compare (dateDay a) (dateDay b)

formatDateMonDD :: Date -> T.Text
formatDateMonDD date = T.pack (show (dateMonth date)) <> " " <> day
  where
    day =
      let d = dateDay date
       in if T.length (showt d) == 1
            then "0" <> showt d
            else showt d

formateDateMonDDYear :: Date -> T.Text
formateDateMonDDYear date = formatDateMonDD date <> ", " <> showt (dateYear date)

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

-- | Parse a date string in YYYY-MM-DD format.
parseDate :: T.Text -> Maybe Date
parseDate = parse . T.splitOn "-"
  where
    parse :: [T.Text] -> Maybe Date
    parse [y, m, d] = do
      year <- parseYear (T.unpack y)
      month <- parseMonth (T.unpack m)
      day <- parseDay month (T.unpack d)
      return $ Date {dateYear = year, dateMonth = month, dateDay = day}
    parse _ = Nothing

-- | Extract the year from a date string in YYYY-MM-DD format.
getYear :: T.Text -> Maybe Int
getYear = fmap dateYear . parseDate
