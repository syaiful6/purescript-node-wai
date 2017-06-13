module Network.Wai.Internal where

import Prelude

import Control.Monad.Aff (Aff)

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.List (List)
import Data.Maybe (Maybe)

import Network.Wai.Types as H

newtype Request eff = Request
  { method         :: H.Method
  -- ^ Request method, such as GET
  , headers        :: H.RequestHeaders
  -- ^ Request headers
  , httpVersion    :: H.HttpVersion
  -- ^ HTTP version such as 1.1.
  , rawPathInfo    :: String
  -- ^ All information after the domain name
  , rawQueryString :: String
  -- ^ If no query string was specified, this should be empty. This value will include
  -- the leading question mark.
  , query          :: H.Query
  -- ^ Parsed query string information.
  , pathInfo       :: List String
  -- ^ Path info in individual pieces - the URL without a hostname/port and without
  -- a query string, split on forward slashes.
  , body           :: Aff eff ByteString
  -- ^ Get the next chunk of the body. Return empty ByteString when the body fully
  -- consumed
  }

data Response eff
  = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
  | ResponseBuilder H.Status H.ResponseHeaders Builder
  | ResponseStream H.Status H.ResponseHeaders (StreamingBody eff)
  | ResponseRaw (Aff eff ByteString -> (ByteString -> Aff eff Unit) -> Aff eff Unit) (Response eff)

type StreamingBody eff = (Builder -> Aff eff Unit) -> Aff eff Unit -> Aff eff Unit

newtype FilePart = FilePart
  { offset    :: Int
  , byteCount :: Int
  , size      :: Int
  }

type FilePath = String
