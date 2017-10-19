module Network.Wai.Handler.Node.Header
  ( IndexedHeader
  , RequestHeaderKey(..)
  , indexRequestHeader
  , indexResponseHeader
  , defaultIxReqHdrs
  ) where

import Prelude

import Data.Enum (class Enum, class BoundedEnum, toEnum, fromEnum, Cardinality(..))
import Data.Foldable (foldl)
import Data.List (List)
import Data.IntMap as IM
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..))

import Network.Wai.Types (Header, HeaderName, RequestHeaders, ResponseHeaders)


type IndexedHeader = IM.IntMap String

data RequestHeaderKey
  = ReqContentLength
  | ReqTransferEncoding
  | ReqExpect
  | ReqConnection
  | ReqRange
  | ReqHost
  | ReqIfModifiedSince
  | ReqIfUnmodifiedSince
  | ReqIfRange
  | ReqReferer
  | ReqUserAgent

derive instance eqRequestHeaderKey :: Eq RequestHeaderKey
derive instance ordRequestHeaderKey :: Ord RequestHeaderKey

instance boundedRequestHeaderKey :: Bounded RequestHeaderKey where
  bottom = ReqContentLength
  top = ReqUserAgent

instance enumRequestHeaderKey :: Enum RequestHeaderKey where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumRequestHeaderKey :: BoundedEnum RequestHeaderKey where
  cardinality = Cardinality 11
  toEnum = case _ of
    0 -> Just ReqContentLength
    1 -> Just ReqTransferEncoding
    2 -> Just ReqExpect
    3 -> Just ReqConnection
    4 -> Just ReqRange
    5 -> Just ReqHost
    6 -> Just ReqIfModifiedSince
    7 -> Just ReqIfUnmodifiedSince
    8 -> Just ReqIfRange
    9 -> Just ReqReferer
    10 -> Just ReqUserAgent
    _ -> Nothing
  fromEnum = case _ of
    ReqContentLength -> 0
    ReqTransferEncoding -> 1
    ReqExpect -> 2
    ReqConnection -> 3
    ReqRange -> 4
    ReqHost -> 5
    ReqIfModifiedSince -> 6
    ReqIfUnmodifiedSince -> 7
    ReqIfRange -> 8
    ReqReferer -> 9
    ReqUserAgent -> 10

data ResponseHeaderKey
  = ResContentLength
  | ResServer
  | ResDate

derive instance eqResponseHeaderKey :: Eq ResponseHeaderKey
derive instance ordResponseHeaderKey :: Ord ResponseHeaderKey

instance boundedResponseHeaderKey :: Bounded ResponseHeaderKey where
  bottom = ResContentLength
  top    = ResDate

instance enumResponseHeaderKey :: Enum ResponseHeaderKey where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumResponseHeaderKey :: BoundedEnum ResponseHeaderKey where
  cardinality = Cardinality 3
  toEnum = case _ of
    0 -> Just ResContentLength
    1 -> Just ResServer
    2 -> Just ResDate
    _ -> Nothing
  fromEnum = case _ of
    ResContentLength -> 0
    ResServer -> 1
    ResDate -> 2

indexRequestHeader :: RequestHeaders -> IndexedHeader
indexRequestHeader = traverseHeader requestKeyIndex

indexResponseHeader :: ResponseHeaders -> IndexedHeader
indexResponseHeader = traverseHeader responseKeyIndex

defaultIxReqHdrs :: IndexedHeader
defaultIxReqHdrs = IM.empty

requestKeyIndex :: HeaderName -> Maybe Int
requestKeyIndex bs = case S.length (unwrap bs) of
  4  -> if bs == wrap "host" then Just $ fromEnum ReqHost else Nothing
  5  -> if bs == wrap "range" then Just $ fromEnum ReqRange else Nothing
  6  -> if bs == wrap "expect" then Just $ fromEnum ReqExpect else Nothing
  7  -> if bs == wrap "referer" then Just $ fromEnum ReqReferer else Nothing
  8  -> if bs == wrap "if-range" then Just $ fromEnum ReqIfRange else Nothing
  10 -> if bs == wrap "user-agent" then Just $ fromEnum ReqUserAgent else
       if bs == wrap "connection" then Just $ fromEnum ReqConnection else Nothing
  14 -> if bs == wrap "content-length" then Just $ fromEnum ReqContentLength else Nothing
  17 -> if bs == wrap "transfer-encoding" then Just $ fromEnum ReqTransferEncoding else
       if bs == wrap "if-modified-since" then Just $ fromEnum ReqIfModifiedSince
       else Nothing
  19 -> if bs == CaseInsensitiveString "if-unmodified-since" then Just $ fromEnum ReqIfUnmodifiedSince else Nothing
  _  -> Nothing

responseKeyIndex :: HeaderName -> Int
responseKeyIndex bs = case S.length (unwrap bs) of
    4  -> if bs == wrap "date" then Just $ fromEnum ResDate else Nothing
    6  -> if bs == wrap "server" then Just $ fromEnum ResServer else Nothing
    14 -> if bs == wrap "content-length" then Just $ fromEnum ResContentLength else Nothing
    _  -> Nothing

traverseHeader :: (HeaderName -> Maybe Int) -> List Header -> IndexedHeader
traverseHeader f = foldl insert IM.empty
  where
  insert m (Tuple k v) = case f k of
    Nothing -> m
    Just ix -> IM.insert ix v m
