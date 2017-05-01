module Network.HTTP.Types.Date
  ( HTTPDate
  , unHTTPDate
  , fromDateTime
  , fromJSDate
  , parseHTTPDate
  , formatHTTPDate
  , toDateTime
  , rfc1123Date
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.Date as DD
import Data.DateTime (DateTime(..), Year, Weekday(..), Month(..), Day, Hour, Minute, Second,
  date, time)
import Data.Char (toCharCode)
import Data.Enum (toEnum, fromEnum)
import Data.Either (either)
import Data.JSDate as JD
import Data.Maybe (Maybe(..))
import Data.Time as DT

import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.String (string, char, anyDigit)


-- | HTTP Date, this mostly like DateTime. But we wrap this to newtype to allow easy
-- access to weekDay and their component. This data type have instance Eq, Ord and Show to
-- support comparison and debug
newtype HTTPDate = HTTPDate
  { year    :: Year
  , month   :: Month
  , day     :: Day
  , hour    :: Hour
  , minute  :: Minute
  , second  :: Second
  , weekDay :: Weekday
  }

derive instance eqHTTPDate :: Eq HTTPDate
derive instance ordHTTPDate :: Ord HTTPDate

instance showHTTPDate :: Show HTTPDate where
  show s = "(HTTPDate " <> formatHTTPDate s <> " )"

type HTTPDateRec =
  { year :: Year
  , month :: Month
  , day :: Day
  , hour   :: Hour
  , minute :: Minute
  , second :: Second
  , weekDay :: Weekday
  }

type DateRec =
  { year :: Year
  , month :: Month
  , day :: Day
  }

type TimeRec =
  { hour   :: Hour
  , minute :: Minute
  , second :: Second
  }

unHTTPDate :: HTTPDate -> HTTPDateRec
unHTTPDate (HTTPDate d) = d

-- | Convert DateTime to HTTPDate, the millisecond component is lost after this convertion
fromDateTime :: DateTime -> HTTPDate
fromDateTime d =
  let
    dt = date d
    ti = time d
  in
    HTTPDate $
      { year: DD.year dt
      , month: DD.month dt
      , day: DD.day dt
      , hour: DT.hour ti
      , minute: DT.minute ti
      , second: DT.second ti
      , weekDay: DD.weekday dt
      }

toDateTime :: HTTPDate -> DateTime
toDateTime (HTTPDate rec) =
  let
    date = DD.canonicalDate rec.year rec.month rec.day
    time = DT.Time rec.hour rec.minute rec.second bottom
  in
    DateTime date time

fromJSDate :: JD.JSDate -> Maybe HTTPDate
fromJSDate = map fromDateTime <<< JD.toDateTime

parseHTTPDate :: String -> Maybe HTTPDate
parseHTTPDate = either (const Nothing) Just <<< runParser rfc1123Date

-- | Format HTTPDate to rfc1123
formatHTTPDate :: HTTPDate -> String
formatHTTPDate (HTTPDate r) = weekDay2String r.weekDay
  <> ", "
  <> show (fromEnum r.day)
  <> " "
  <> month2String r.month
  <> " "
  <> show (fromEnum r.year)
  <> " "
  <> show (fromEnum r.hour)
  <> ":"
  <> show (fromEnum r.minute)
  <> ":"
  <> show (fromEnum r.second)
  <> " GMT"

rfc1123Date :: Parser HTTPDate
rfc1123Date = do
  w <- parseWeekDay
  void $ string ", "
  dr <- parseDate
  sp
  tr <- parseTime
  sp
  -- RFC 2616 defines GMT only but there are actually ill-formed ones such
  -- as "+0000" and "UTC" in the wild.
  void $ string "GMT" <|> string "+0000" <|> string "UTC"
  pure $ HTTPDate
    { year: dr.year
    , month: dr.month
    , day: dr.day
    , hour: tr.hour
    , minute: tr.minute
    , second: tr.second
    , weekDay: w
    }

parseDate :: Parser DateRec
parseDate = do
  d <- failIfNothing "Invalid Day" (toEnum <$> digit2)
  sp
  m <- parseMonth
  sp
  y <- failIfNothing "Invalid Year" (toEnum <$> digit4)
  pure $ {year: y, month: m, day: d }

parseTime :: Parser TimeRec
parseTime = do
  h <- failIfNothing "Invalid Hour" (toEnum <$> digit2)
  void $ char ':'
  m <- failIfNothing "Invalid Minute" (toEnum <$> digit2)
  void $ char ':'
  s <- failIfNothing "Invalid Minute" (toEnum <$> digit2)
  pure $ { hour: h, minute: m, second: s }

parseWeekDay :: Parser Weekday
parseWeekDay = Monday <$ string "Mon"
  <|> Tuesday    <$ string "Tue"
  <|> Wednesday  <$ string "Wed"
  <|> Thursday   <$ string "Thu"
  <|> Friday     <$ string "Fri"
  <|> Saturday   <$ string "Sat"
  <|> Sunday     <$ string "Sun"

parseMonth :: Parser Month
parseMonth = January <$ string "Jan"
  <|> February  <$ string "Feb"
  <|> March     <$ string "Mar"
  <|> April     <$ string "Apr"
  <|> May       <$ string "May"
  <|> June      <$ string "Jun"
  <|> July      <$ string "Jul"
  <|> August    <$ string "Aug"
  <|> September <$ string "Sep"
  <|> October   <$ string "Oct"
  <|> November  <$ string "Nov"
  <|> December  <$ string "Dec"

sp :: Parser Unit
sp = unit <$ char ' '

digit2 :: Parser Int
digit2 = format2 <$> (char2Int <$> anyDigit) <*> (char2Int <$> anyDigit)
  where
    format2 x y = x * 10 + y

digit4 :: Parser Int
digit4 = do
  x1 <- char2Int <$> anyDigit
  x2 <- char2Int <$> anyDigit
  x3 <- char2Int <$> anyDigit
  x4 <- char2Int <$> anyDigit
  pure $ x1 * 1000 + x2 * 100 + x3 * 10 + x4

char2Int :: Char -> Int
char2Int s = toCharCode s - toCharCode '0'

failIfNothing :: forall a. String -> Parser (Maybe a) -> Parser a
failIfNothing s parser = do
  d <- parser
  case d of
    Nothing -> fail s
    Just d' -> pure d'

weekDay2String :: Weekday -> String
weekDay2String Monday    = "Mon"
weekDay2String Tuesday   = "Tue"
weekDay2String Wednesday = "Wed"
weekDay2String Thursday  = "Thu"
weekDay2String Friday    = "Fri"
weekDay2String Saturday  = "Sat"
weekDay2String Sunday    = "Sun"

month2String :: Month -> String
month2String January    = "Jan"
month2String February   = "Feb"
month2String March      = "Mar"
month2String April      = "Apr"
month2String May        = "May"
month2String June       = "Jun"
month2String July       = "Jul"
month2String August     = "Aug"
month2String September  = "Sep"
month2String October    = "Oct"
month2String November   = "Nov"
month2String December   = "Dec"
