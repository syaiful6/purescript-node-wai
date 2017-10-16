module Network.Wai.Handler.Node.File
  ( RspFileInfo(..)
  , conditionalRequest
  , addContentHeadersForFilePart
  ) where

import Prelude

import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.IntMap as IM
import Data.List (List(Nil), (:))
import Data.Tuple (Tuple(..))

import Network.Wai (FilePart(..))
import Network.Wai.Types as H
import Network.Wai.Types.Header (parseByteRanges)
import Network.Wai.Types.Date (HTTPDate, parseHTTPDate)
import Network.Wai.Handler.Node.Header (RequestHeaderKey(..), IndexedHeader)
import Network.Wai.Handler.Node.FileInfoCache as I


data RspFileInfo
  = WithoutBody H.Status
  | WithBody H.Status H.ResponseHeaders Int Int

derive instance eqRspFileInfo :: Eq RspFileInfo

conditionalRequest :: I.FileInfo -> H.ResponseHeaders -> IndexedHeader -> RspFileInfo
conditionalRequest (I.FileInfo finfo) hs0 hm = case condition of
  nobody@(WithoutBody _) -> nobody
  WithBody s _ off len ->
    let hs = (Tuple H.hLastModified finfo.date : Nil) <> addContentHeaders hs0 off len finfo.size
    in WithBody s hs off len
  where
    -- manually check, to avoid unnecessary evaluation
    mcondition = case ifmodified hm finfo.size finfo.time of
      ifm@Just _ -> ifm
      Nothing    -> case ifunmodified hm finfo.size finfo.time of
        ifun@Just _  -> ifun
        Nothing      -> ifrange hm finfo.size finfo.time
    condition = fromMaybe (unconditional hm finfo.size) mcondition

ifModifiedSince :: IndexedHeader -> Maybe HTTPDate
ifModifiedSince hm = IM.lookup (fromEnum ReqIfModifiedSince) hm >>= parseHTTPDate

ifUnmodifiedSince :: IndexedHeader -> Maybe HTTPDate
ifUnmodifiedSince hm = IM.lookup (fromEnum ReqIfUnmodifiedSince) hm >>= parseHTTPDate

ifRange :: IndexedHeader -> Maybe HTTPDate
ifRange hm = IM.lookup (fromEnum ReqIfRange) hm >>= parseHTTPDate

ifmodified :: IndexedHeader -> Int -> HTTPDate -> Maybe RspFileInfo
ifmodified hm size mtime = do
  date <- ifModifiedSince hm
  pure $ if date /= mtime then unconditional hm size else WithoutBody H.status304

ifunmodified :: IndexedHeader -> Int -> HTTPDate -> Maybe RspFileInfo
ifunmodified hm size mtime = do
  date <- ifUnmodifiedSince hm
  pure $ if date == mtime then unconditional hm size else WithoutBody H.status412

ifrange :: IndexedHeader -> Int -> HTTPDate -> Maybe RspFileInfo
ifrange hm size mtime = do
  date <- ifRange hm
  rng <- IM.lookup (fromEnum ReqRange) hm
  pure $ if date == mtime then parseRange rng size else WithBody H.status200 Nil 0 size

unconditional :: IndexedHeader -> Int -> RspFileInfo
unconditional hm size = case IM.lookup (fromEnum ReqRange) hm of
  Nothing  -> WithBody H.status200 Nil 0 size
  Just rng -> parseRange rng size

parseRange :: String -> Int -> RspFileInfo
parseRange rn size = case parseByteRanges rn of
  Nothing     -> WithoutBody H.status416
  Just Nil    -> WithoutBody H.status416
  Just (r:_)  ->
    let Tuple beg end = checkRange r size
        len           = end - beg + 1
        s             = if beg == 0 && end == size - 1 then H.status200 else H.status206
    in WithBody s Nil beg len

checkRange :: H.ByteRange -> Int -> Tuple Int Int
checkRange (H.ByteRangeFrom   beg)     size = Tuple beg (size - 1)
checkRange (H.ByteRangeFromTo beg end) size = Tuple beg (min (size - 1) end)
checkRange (H.ByteRangeSuffix count)   size = Tuple (max 0 (size - count)) (size - 1)

contentRangeHeader :: Int -> Int -> Int -> H.Header
contentRangeHeader beg end total = Tuple H.hContentRange byterange
  where
    byterange = "bytes " <> (if beg > end then "*" else show beg <> "-" <> show end) <> "/" <> show total

addContentHeaders :: H.ResponseHeaders -> Int -> Int -> Int -> H.ResponseHeaders
addContentHeaders hs off len size =
  if len == size
    then hs'
    else
      let cr = contentRangeHeader off (off + len - 1) size
      in (cr : Nil) <> hs'
  where
    hs' = ( Tuple H.hContentLength (show len)
          : Tuple H.hAcceptRanges "bytes"
          : Nil
          ) <> hs

addContentHeadersForFilePart :: H.ResponseHeaders -> FilePart -> H.ResponseHeaders
addContentHeadersForFilePart hs (FilePart fp) = addContentHeaders hs off len size
  where
    off = fp.offset
    len = fp.byteCount
    size = fp.size
