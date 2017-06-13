module Network.Wai
  ( Application
  , Middleware
  -- Build response
  , responseFile
  , responseBuilder
  , responseLBS
  , responseStream
  , responseRaw
  -- Accessing response
  , responseStatus
  , responseHeaders
  --
  , defaultRequest
  , modifyResponse
  , ifRequest
  , module Network.Wai.Internal
  ) where

import Prelude

import Control.Monad.Aff (Aff)

import Data.ByteString.Builder (Builder)
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Data.ByteString.Builder.Internal (lazyByteStringInsert)
import Data.List (List(Nil))
import Data.Maybe (Maybe)

import Network.Wai.Internal (Request(..), Response(..), StreamingBody, FilePart(..), FilePath)
import Network.Wai.Types as H


-- | Creating 'Response' from a file.
responseFile
  :: forall eff
   . H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> Response eff
responseFile = ResponseFile

responseBuilder
  :: forall eff
   . H.Status -> H.ResponseHeaders -> Builder -> Response eff
responseBuilder = ResponseBuilder

responseLBS
 :: forall eff
  . H.Status -> H.ResponseHeaders -> L.ByteString -> Response eff
responseLBS s h = ResponseBuilder s h <<< lazyByteStringInsert

responseStream
  :: forall eff
   . H.Status -> H.ResponseHeaders -> StreamingBody eff -> Response eff
responseStream = ResponseStream

responseRaw
  :: forall eff
   . (Aff eff B.ByteString -> (B.ByteString -> Aff eff Unit) -> Aff eff Unit)
  -> Response eff
  -> Response eff
responseRaw = ResponseRaw

responseStatus :: forall eff. Response eff -> H.Status
responseStatus (ResponseFile    s _ _ _) = s
responseStatus (ResponseBuilder s _ _  ) = s
responseStatus (ResponseStream  s _ _  ) = s
responseStatus (ResponseRaw _ res      ) = responseStatus res

-- | Accessing 'H.ResponseHeaders' in 'Response'.
responseHeaders :: forall eff. Response eff -> H.ResponseHeaders
responseHeaders (ResponseFile    _ hs _ _) = hs
responseHeaders (ResponseBuilder _ hs _  ) = hs
responseHeaders (ResponseStream  _ hs _  ) = hs
responseHeaders (ResponseRaw _ res)        = responseHeaders res

-- | Apply the provided function to the response header list of the Response.
mapResponseHeaders
  :: forall eff
   . (H.ResponseHeaders -> H.ResponseHeaders) -> Response eff -> Response eff
mapResponseHeaders f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapResponseHeaders f (ResponseBuilder s h b)  = ResponseBuilder s (f h) b
mapResponseHeaders f (ResponseStream s h b)   = ResponseStream s (f h) b
mapResponseHeaders _ r@(ResponseRaw _ _)      = r

-- | Apply the provided function to the response status of the Response.
mapResponseStatus
  :: forall eff
   . (H.Status -> H.Status) -> Response eff -> Response eff
mapResponseStatus f (ResponseFile s h b1 b2) = ResponseFile (f s) h b1 b2
mapResponseStatus f (ResponseBuilder s h b) = ResponseBuilder (f s) h b
mapResponseStatus f (ResponseStream s h b) = ResponseStream (f s) h b
mapResponseStatus _ r@(ResponseRaw _ _) = r

-- | The signature of Wai Application. Type r used to make sure the application
-- | call the continuation.
type Application eff = forall r. Request eff -> (Response eff -> Aff eff r) -> Aff eff r

defaultRequest :: forall eff. Request eff
defaultRequest = Request
  { method: H.GET
  , headers: Nil
  , httpVersion: H.http10
  , rawPathInfo: ""
  , rawQueryString: ""
  , query: Nil
  , pathInfo: Nil
  , body: pure B.empty
  }

type Middleware eff = Application eff -> Application eff

modifyResponse :: forall eff. (Response eff -> Response eff) -> Middleware eff
modifyResponse f app req respond = app req (respond <<< f)

ifRequest :: forall eff. (Request eff -> Boolean) -> Middleware eff -> Middleware eff
ifRequest rpred middle app req | rpred req = middle app req
                               | otherwise =        app req
