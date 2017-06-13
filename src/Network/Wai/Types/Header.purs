module Network.Wai.Types.Header
  ( Header
  , HeaderName
  , RequestHeaders
  , ResponseHeaders
    -- ** Common headers
  , hAccept
  , hAcceptCharset
  , hAcceptEncoding
  , hAcceptLanguage
  , hAcceptRanges
  , hAge
  , hAllow
  , hAuthorization
  , hCacheControl
  , hConnection
  , hContentEncoding
  , hContentLanguage
  , hContentLength
  , hContentLocation
  , hContentMD5
  , hContentRange
  , hContentType
  , hDate
  , hETag
  , hExpect
  , hExpires
  , hFrom
  , hHost
  , hIfMatch
  , hIfModifiedSince
  , hIfNoneMatch
  , hIfRange
  , hIfUnmodifiedSince
  , hLastModified
  , hLocation
  , hMaxForwards
  , hPragma
  , hProxyAuthenticate
  , hProxyAuthorization
  , hRange
  , hReferer
  , hRetryAfter
  , hServer
  , hTE
  , hTrailer
  , hTransferEncoding
  , hUpgrade
  , hUserAgent
  , hVary
  , hVia
  , hWWWAuthenticate
  , hWarning
  , hContentDisposition
  , hMIMEVersion
  , hCookie
  , hSetCookie
    -- ** Byte ranges
  , ByteRange(..)
  , renderByteRange
  , renderByteRanges
  , parseByteRanges
  ) where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple (Tuple(..))
import Data.Function.Uncurried (Fn4, runFn4)


-- | Header name
type HeaderName = CaseInsensitiveString

-- | Header
type Header = Tuple HeaderName String

-- RequestHeaders
type RequestHeaders  = List Header

-- ResponseHeaders
type ResponseHeaders =  List Header

-- | HTTP Header names according to http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
hAccept :: HeaderName
hAccept = CaseInsensitiveString "Accept"
hAcceptCharset :: HeaderName
hAcceptCharset = CaseInsensitiveString "Accept-Charset"
hAcceptEncoding :: HeaderName
hAcceptEncoding = CaseInsensitiveString "Accept-Encoding"
hAcceptLanguage :: HeaderName
hAcceptLanguage = CaseInsensitiveString "Accept-Language"
hAcceptRanges :: HeaderName
hAcceptRanges = CaseInsensitiveString "Accept-Ranges"
hAge :: HeaderName
hAge = CaseInsensitiveString "Age"
hAllow :: HeaderName
hAllow = CaseInsensitiveString "Allow"
hAuthorization :: HeaderName
hAuthorization = CaseInsensitiveString "Authorization"
hCacheControl :: HeaderName
hCacheControl = CaseInsensitiveString "Cache-Control"
hConnection :: HeaderName
hConnection = CaseInsensitiveString "Connection"
hContentEncoding :: HeaderName
hContentEncoding = CaseInsensitiveString "Content-Encoding"
hContentLanguage :: HeaderName
hContentLanguage = CaseInsensitiveString "Content-Language"
hContentLength :: HeaderName
hContentLength = CaseInsensitiveString "Content-Length"
hContentLocation :: HeaderName
hContentLocation = CaseInsensitiveString "Content-Location"
hContentMD5 :: HeaderName
hContentMD5 = CaseInsensitiveString "Content-MD5"
hContentRange :: HeaderName
hContentRange = CaseInsensitiveString "Content-Range"
hContentType :: HeaderName
hContentType = CaseInsensitiveString "Content-Type"
hDate :: HeaderName
hDate = CaseInsensitiveString "Date"
hETag :: HeaderName
hETag = CaseInsensitiveString "ETag"
hExpect :: HeaderName
hExpect = CaseInsensitiveString "Expect"
hExpires :: HeaderName
hExpires = CaseInsensitiveString "Expires"
hFrom :: HeaderName
hFrom = CaseInsensitiveString "From"
hHost :: HeaderName
hHost = CaseInsensitiveString "Host"
hIfMatch :: HeaderName
hIfMatch = CaseInsensitiveString "If-Match"
hIfModifiedSince :: HeaderName
hIfModifiedSince = CaseInsensitiveString "If-Modified-Since"
hIfNoneMatch :: HeaderName
hIfNoneMatch = CaseInsensitiveString "If-None-Match"
hIfRange :: HeaderName
hIfRange = CaseInsensitiveString "If-Range"
hIfUnmodifiedSince :: HeaderName
hIfUnmodifiedSince = CaseInsensitiveString "If-Unmodified-Since"
hLastModified :: HeaderName
hLastModified = CaseInsensitiveString "Last-Modified"
hLocation :: HeaderName
hLocation = CaseInsensitiveString "Location"
hMaxForwards :: HeaderName
hMaxForwards = CaseInsensitiveString "Max-Forwards"
hPragma :: HeaderName
hPragma = CaseInsensitiveString "Pragma"
hProxyAuthenticate :: HeaderName
hProxyAuthenticate = CaseInsensitiveString "Proxy-Authenticate"
hProxyAuthorization :: HeaderName
hProxyAuthorization = CaseInsensitiveString "Proxy-Authorization"
hRange :: HeaderName
hRange = CaseInsensitiveString "Range"
hReferer :: HeaderName
hReferer = CaseInsensitiveString "Referer"
hRetryAfter :: HeaderName
hRetryAfter = CaseInsensitiveString "Retry-After"
hServer :: HeaderName
hServer = CaseInsensitiveString "Server"
hTE :: HeaderName
hTE = CaseInsensitiveString "TE"
hTrailer :: HeaderName
hTrailer = CaseInsensitiveString "Trailer"
hTransferEncoding :: HeaderName
hTransferEncoding = CaseInsensitiveString "Transfer-Encoding"
hUpgrade :: HeaderName
hUpgrade = CaseInsensitiveString "Upgrade"
hUserAgent :: HeaderName
hUserAgent = CaseInsensitiveString "User-Agent"
hVary :: HeaderName
hVary = CaseInsensitiveString "Vary"
hVia :: HeaderName
hVia = CaseInsensitiveString "Via"
hWWWAuthenticate :: HeaderName
hWWWAuthenticate = CaseInsensitiveString "WWW-Authenticate"
hWarning :: HeaderName
hWarning = CaseInsensitiveString "Warning"

-- | HTTP Header names according to http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html
hContentDisposition :: HeaderName
hContentDisposition = CaseInsensitiveString "Content-Disposition"
hMIMEVersion :: HeaderName
hMIMEVersion = CaseInsensitiveString "MIME-Version"

-- | HTTP Header names according to https://tools.ietf.org/html/rfc6265#section-4
hCookie :: HeaderName
hCookie = CaseInsensitiveString "Cookie"
hSetCookie :: HeaderName
hSetCookie = CaseInsensitiveString "Set-Cookie"

data ByteRange
  = ByteRangeFrom Int
  | ByteRangeFromTo Int Int
  | ByteRangeSuffix Int

derive instance eqByteRange :: Eq ByteRange

instance showByteRange :: Show ByteRange where
  show (ByteRangeFrom b) = "(ByteRangeFrom " <> show b <> " )"
  show (ByteRangeFromTo a b) = "(ByteRangeFromTo " <> show a <> " " <> show b <> " )"
  show (ByteRangeSuffix co) = "(ByteRangeSuffix " <> show co <> " )"

renderByteRange :: ByteRange -> String
renderByteRange (ByteRangeFrom beg)      = show beg <> "-"
renderByteRange (ByteRangeFromTo beg to) = show beg <> "-" <> show to
renderByteRange (ByteRangeSuffix suffix) = "-" <> show suffix

renderByteRanges :: forall f. Foldable f => Functor f => f ByteRange -> String
renderByteRanges xs = "bytes="
  <> intercalate "," (map renderByteRange xs)

parseByteRanges :: String -> Maybe (List ByteRange)
parseByteRanges bs1 = do
  bs2 <- S.stripPrefix (S.Pattern "bytes=") bs1
  Tuple r bs3 <- range bs2
  ranges ((:) r) bs3
  where
    range bs2 = do
      Tuple i bs3 <- readInteger bs2
      if i < 0
        then Just $ Tuple (ByteRangeSuffix (negate i)) bs3
        else do
          bs4 <- S.stripPrefix (S.Pattern "-") bs3
          case readInteger bs4 of
            Just (Tuple j bs5) | j >= i -> Just $ Tuple (ByteRangeFromTo i j) bs5
            _ -> Just $ Tuple (ByteRangeFrom i) bs4
    ranges front bs3
      | bs3 == "" = Just (front Nil)
      | otherwise = do
          bs4 <- S.stripPrefix (S.Pattern ",") bs3
          Tuple r bs5 <- range bs4
          ranges (front <<< ((:) r)) bs5

readInteger :: String -> Maybe (Tuple Int String)
readInteger st = runFn4 readIntegerImpl Nothing Just Tuple st

foreign import readIntegerImpl
  :: Fn4
      (forall a. Maybe a)
      (forall a. a -> Maybe a)
      (forall a b. a -> b -> Tuple a b)
      String
      (Maybe (Tuple Int String))
