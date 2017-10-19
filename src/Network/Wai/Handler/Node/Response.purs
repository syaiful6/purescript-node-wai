module Network.Wai.Handler.Node.Response where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (try)

import Data.Array as A
import Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString, string7)
import Data.ByteString.Builder.Extra (refinedEff, flush, newByteStringBuilderRecv, reuseBufferStrategy)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (intercalate)
import Data.Function (on)
import Data.IntMap as IM
import Data.List (List(Nil), (:), deleteBy)
import Data.String as S
import Data.Tuple (Tuple(..), fst)

import Node.Path (FilePath)

import Network.Wai (Request(..), Response(..), StreamingBody, responseStatus
                   ,responseHeaders, FilePart(..))
import Network.Wai.Types as H
import Network.Wai.Handler.Node.Buffer (chunkedTransferEncoding, chunkedTransferTerminator
                                       ,toBufAffWith, toBuilderBuffer)
import Network.Wai.Handler.Node.Date as D
import Network.Wai.Handler.Node.File (RspFileInfo(..), conditionalRequest, addContentHeadersForFilePart)
import Network.Wai.Handler.Node.Types as Z
import Network.Wai.Handler.Node.Timeout as T
import Network.Wai.Handler.Node.Header (ResponseHeaderKey(..), RequestHeaderKey(..)
                                       ,IndexedHeader, indexResponseHeader)
import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.ResponseHeader (composeHeader)
import Network.Wai.Handler.Node.Utils (nextTick)


sendResponse
  :: forall eff
   . Z.Settings (WaiEffects eff)
  -> Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> Request (WaiEffects eff)
  -> IndexedHeader
  -> Aff (WaiEffects eff) B.ByteString
  -> Response (WaiEffects eff)
  -> Aff (WaiEffects eff) Unit
sendResponse settings conn ii request@(Request req) reqidxhdr src response = do
  hs <- addServerAndDate hs0
  if hasBody s
    then do
      Tuple ms mlen <- sendRsp conn ii ver s hs rsp
      case ms of
        Nothing -> pure unit
        Just realStatus -> logger request realStatus mlen
      T.tickle th
    else do
      _ <- sendRsp conn ii ver s hs RspNoBody
      logger request s Nothing
      T.tickle th
  where
    logger = Z.settingsLogger settings
    ver = req.httpVersion
    s = responseStatus response
    getdate = Z.getDate ii
    addServerAndDate = addDate getdate rspidxhdr
    hs0 = sanitizeHeaders $ responseHeaders response
    rspidxhdr = indexResponseHeader hs0
    th     = Z.timeoutHandle ii
    Tuple isPersist isChunked0 = infoFromRequest request reqidxhdr
    isHead    = req.method == H.HEAD
    isChunked = not isHead && isChunked0
    Tuple isKeepAlive needsChunked = infoFromResponse rspidxhdr (Tuple isPersist isChunked)
    rsp    = case response of
      ResponseFile _ _ path mPart -> RspFile path mPart reqidxhdr isHead (T.tickle th)
      ResponseBuilder _ _ b
        | isHead                  -> RspNoBody
        | otherwise               -> RspBuilder b needsChunked
      ResponseStream _ _ fb
        | isHead                  -> RspNoBody
        | otherwise               -> RspStream fb needsChunked th
      ResponseRaw raw _           -> RspRaw raw src (T.tickle th)
    ret = case response of
        ResponseFile    _ _ _ _ -> isPersist
        ResponseBuilder _ _ _   -> isKeepAlive
        ResponseStream  _ _ _   -> isKeepAlive
        ResponseRaw     _ _     -> false

data Rsp eff
  = RspNoBody
  | RspFile FilePath (Maybe FilePart) IndexedHeader Boolean (Aff eff Unit)
  | RspBuilder Builder Boolean
  | RspStream (StreamingBody eff) Boolean (T.Handle eff)
  | RspRaw (Aff eff B.ByteString -> (B.ByteString -> Aff eff Unit) -> Aff eff Unit) (Aff eff B.ByteString) (Aff eff Unit)

sanitizeHeaders :: H.ResponseHeaders -> H.ResponseHeaders
sanitizeHeaders = map (map sanitize)
  where
    sanitize v
      | containsNewlines v = sanitizeHeaderValue v
      | otherwise          = v

containsNewlines :: String -> Boolean
containsNewlines s = S.contains (S.Pattern "\r") s
                  || S.contains(S.Pattern "\n") s

sanitizeHeaderValue :: String -> String
sanitizeHeaderValue s = case A.uncons xss of
  Nothing             -> ""
  Just { head, tail } -> intercalate "\r\n" (A.cons head (A.mapMaybe addSpaceIfMissing tail))
  where
  addSpaceIfMissing line = case S.uncons line of
    Nothing -> Nothing
    Just { head }
      | head == ' ' || head == '\t' -> Just line
      | otherwise                   -> Just (" " <> line)

  xss = linesStr $ S.replaceAll (S.Pattern "\r") (S.Replacement "") s

linesStr :: String -> Array String
linesStr str = go xsc
  where
  go xs =
    if A.length xs == 0
      then []
      else
        let { init, rest } = A.span (not <<< pred) xs
        in A.cons (S.fromCharArray init) $ maybe [] go (A.tail rest)
  xsc = S.toCharArray str
  pred x = eq x '\n'

sendRsp
  :: forall eff
   . Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> H.HttpVersion
  -> H.Status
  -> H.ResponseHeaders
  -> Rsp (WaiEffects eff)
  -> Aff (WaiEffects eff) (Tuple (Maybe H.Status) (Maybe Int))
sendRsp (Z.Connection { connSendAll }) _ ver s hs RspNoBody = do
  _ <- liftEff (composeHeader ver s hs) >>= connSendAll
  pure (Tuple (Just s) Nothing)
sendRsp (Z.Connection conn) _ ver s hs (RspBuilder body needsChunked) = do
  header <- liftEff $ composeHeaderBuilder ver s hs needsChunked
  let hdrBdy
        | needsChunked = header <> chunkedTransferEncoding body
                                <> chunkedTransferTerminator
        | otherwise    = header <> body
      buffer = conn.connWriteBuffer
      size   = conn.connBufferSize
  _ <- toBufAffWith buffer size (conn.connSendAll) hdrBdy
  pure (Tuple (Just s) Nothing)
sendRsp connect@(Z.Connection conn) _ ver s hs (RspStream streamingBody needsChunked th) = do
  header <- liftEff $ composeHeaderBuilder ver s hs needsChunked
  Tuple recv finish <- liftEff $ refinedEff $
                    newByteStringBuilderRecv $ reuseBufferStrategy $
                    pure $ toBuilderBuffer conn.connWriteBuffer (conn.connBufferSize)
  let send builder = do
        popper <- liftEff $ refinedEff $ recv builder
        let go = do
              bs <- liftEff $ refinedEff $ popper
              unless (B.null bs) $ do
                _ <- sendFragment connect th bs
                nextTick
                go
        go
      sendChunk
        | needsChunked = send <<< chunkedTransferEncoding
        | otherwise    = send
  _ <- send header
  _ <- streamingBody sendChunk (sendChunk flush)
  when needsChunked $ send chunkedTransferTerminator
  mbs <- liftEff $ refinedEff $ finish
  _ <- maybe (pure unit) (sendFragment connect th) mbs
  pure (Tuple (Just s) Nothing)
sendRsp (Z.Connection { connSendAll }) _ _ _ _ (RspRaw withApp src tickle) = do
  let
    recv = do
      bs <- src
      unless (B.null bs) tickle
      pure bs
    send bs = connSendAll bs *> tickle
  _ <- withApp recv send
  pure (Tuple Nothing Nothing)
sendRsp conn ii ver s0 hs0 (RspFile path (Just (part@FilePart { offset, byteCount })) _ isHead hook) =
  sendRspFile2XX conn ii ver s0 hs path offset byteCount isHead hook
  where
  hs = addContentHeadersForFilePart hs0 part
sendRsp conn ii ver _ hs0 (RspFile path Nothing idxhdr isHead hook) = do
  efinfo <- try $ Z.getFileInfo ii path
  case efinfo of
    Left err    -> sendRspFile404 conn ii ver hs0
    Right finfo -> case conditionalRequest finfo hs0 idxhdr of
      WithoutBody s         -> sendRsp conn ii ver s hs0 RspNoBody
      WithBody s hs beg len -> sendRspFile2XX conn ii ver s hs path beg len isHead hook

sendRspFile2XX
  :: forall eff
   . Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> H.HttpVersion
  -> H.Status
  -> H.ResponseHeaders
  -> FilePath
  -> Int
  -> Int
  -> Boolean
  -> Aff (WaiEffects eff) Unit
  -> Aff (WaiEffects eff) (Tuple (Maybe H.Status) (Maybe Int))
sendRspFile2XX conn@(Z.Connection { connSendAll, connSendFile }) ii ver s hs path beg len isHead hook
  | isHead    = sendRsp conn ii ver s hs RspNoBody
  | otherwise = do
      Tuple mfd fresher <- Z.getFd ii path
      let
        fid   = Z.FileId { fileIdPath: path, fileIdFd: mfd }
        hook' = hook *> fresher
      bhs <- liftEff $ composeHeader ver s hs
      _ <- connSendAll bhs
      _ <- connSendFile fid beg len hook'
      pure (Tuple (Just s) (Just len))

sendRspFile404
  :: forall eff
   . Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> H.HttpVersion
  -> H.ResponseHeaders
  -> Aff (WaiEffects eff) (Tuple (Maybe H.Status) (Maybe Int))
sendRspFile404 conn ii ver hs0 = sendRsp conn ii ver s hs (RspBuilder body true)
  where
  s    = H.notFound404
  hs   = replaceHeader H.hContentType "text/plain; charset=utf-8" hs0
  body = string7 "File not found"

infoFromRequest :: forall eff. Request eff -> IndexedHeader -> Tuple Boolean Boolean
infoFromRequest req reqidxhdr = Tuple (checkPersist req reqidxhdr) (checkChunk req)

checkPersist :: forall eff. Request eff -> IndexedHeader -> Boolean
checkPersist (Request { httpVersion }) reqidxhdr =
  if httpVersion == H.http11 then checkPersist11 conn else checkPersist10 conn
  where
    conn = fromEnum ReqConnection `IM.lookup` reqidxhdr
    checkPersist11 (Just x)
        | S.toLower x == "close"      = false
    checkPersist11 _                  = true
    checkPersist10 (Just x)
        | S.toLower x == "keep-alive" = true
    checkPersist10 _                  = false

checkChunk :: forall eff. Request eff -> Boolean
checkChunk (Request { httpVersion }) = httpVersion == H.http11

infoFromResponse :: IndexedHeader -> Tuple Boolean Boolean -> Tuple Boolean Boolean
infoFromResponse rspidxhdr (Tuple isPersist isChunked) = Tuple isKeepAlive needsChunked
  where
    needsChunked = isChunked && not hasLength
    isKeepAlive  = isPersist && (isChunked || hasLength)
    hasLength    = fromEnum ResContentLength `IM.member` rspidxhdr

hasBody :: H.Status -> Boolean
hasBody s = sc /= 204
         && sc /= 304
         && sc >= 200
  where
  sc = H.statusNumber s

addTransferEncoding :: H.ResponseHeaders -> H.ResponseHeaders
addTransferEncoding hdrs = (Tuple H.hTransferEncoding "chunked" : hdrs)

addDate :: forall eff . Aff eff D.GMTDate -> IndexedHeader -> H.ResponseHeaders -> Aff eff H.ResponseHeaders
addDate getdate rspidxhdr hdrs = case fromEnum ResDate `IM.lookup` rspidxhdr of
  Nothing -> do
    gmtdate <- getdate
    pure $ Tuple H.hDate gmtdate : hdrs
  Just _ -> pure hdrs

sendFragment
  :: forall e
   . Z.Connection (WaiEffects e)
  -> T.Handle (WaiEffects e)
  -> B.ByteString
  -> Aff (WaiEffects e) Unit
sendFragment (Z.Connection { connSendAll }) th bs = do
  _ <- T.resume th
  _ <- connSendAll bs
  T.pause th

replaceHeader :: H.HeaderName -> String -> H.ResponseHeaders -> H.ResponseHeaders
replaceHeader k v hdrs = (Tuple k v : Nil) <> deleteBy ((==) `on` fst) (Tuple k v) hdrs

composeHeaderBuilder :: forall eff. H.HttpVersion -> H.Status -> H.ResponseHeaders -> Boolean -> Eff eff Builder
composeHeaderBuilder ver s hs true =
    byteString <$> composeHeader ver s (addTransferEncoding hs)
composeHeaderBuilder ver s hs false =
    byteString <$> composeHeader ver s hs
