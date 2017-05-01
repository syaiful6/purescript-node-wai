module Network.HTTP.Wai.Internal
  ( Request(..)
  , Response(..)
  , FilePath
  , FilePart(..)
  , ResponseReceived(..)
  ) where

import Prelude

import Data.Foldable (intercalate)
import Data.URI.Query (Query, printQuery)
import Data.Maybe (Maybe)
import Data.List (List)

import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.Stream (Readable)

import Network.HTTP.Types as H
import Node.HTTP (HTTP)

newtype Request = Request
  { httpVersion :: H.HttpVersion
  , method :: H.Method
  -- this is most likely all information after the domain name
  , rawPathInfo :: String
  -- If no query string was specified, this should be empty. This value
  -- /will/ include the leading question mark.
  , rawQueryString :: String
  -- query sortBy=name&page=2, it parsed using Data.URI.Query
  , query :: Maybe Query
  -- Path info in individual pieces - the URL without a hostname/port and
  -- without a query string, split on forward slashes.
  , pathInfo :: List String
  , headers :: H.RequestHeaders
  , body :: forall e. Readable () (http :: HTTP | e) }

instance showRequest :: Show Request where
  show (Request s) = "Request {" <> intercalate "," fields <> "}"
    where
      fields =
        [ "httpVersion " <> show s.httpVersion
        , "method " <> show s.method
        , "rawPathInfo " <> s.rawPathInfo
        , "rawQueryString " <> s.rawQueryString
        , "query " <> show (printQuery <$> s.query)
        , "headers " <> show s.headers
        , "body <Stream>"
        ]

data Response eff
  = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
  | ResponseStream H.Status H.ResponseHeaders (forall a. Readable a eff)
  | ResponseBuffer H.Status H.ResponseHeaders Buffer
  | ResponseString H.Status H.ResponseHeaders Encoding String

type FilePath = String

newtype FilePart = FilePart
  { offset :: Int
  , byteCount :: Int
  , size :: Int
  }

data ResponseReceived = ResponseReceived
