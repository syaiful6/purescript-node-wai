module Network.Wai.Handler.Node.Response
  ( sendResponse
  , replaceHeader
  , hasBody
  ) where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Eff.Exception (error)

import Data.ArrayBuffer.TypedArray (plusPtr)
import Data.ByteString as B
import Data.ByteString.Builder (Builder, string7)
import Data.ByteString.Builder.Internal as BI
import Data.ByteString.Builder.Extra
  (runBuilder, newByteStringBuilderRecv, reuseBufferStrategy, refinedEff, flush, Next(..))
import Data.Either (Either(..))
import Data.Function (on)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst)
import Data.List (List(Nil), (:), deleteBy)
import Data.Newtype (wrap)

import Node.Path (FilePath)

import Network.Wai
  (Request(..), Response(..), StreamingBody, responseStatus, responseHeaders, FilePart(..))
import Network.Wai.Types as H
import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.Header (IndexedHeader)
import Network.Wai.Handler.Node.Types as Z
import Network.Wai.Handler.Node.Timeout as T
import Network.Wai.Handler.Node.File (RspFileInfo(..), conditionalRequest, addContentHeadersForFilePart)
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
  ver    = req.httpVersion
  hs     = responseHeaders response
  logger = Z.settingsLogger settings
  s      = responseStatus response
  th     = Z.timeoutHandle ii
  isHead = req.method == H.HEAD
  rsp    = case response of
    ResponseFile _ _ path mPart -> RspFile path mPart reqidxhdr isHead (T.tickle th)
    ResponseBuilder _ _ b
      | isHead                  -> RspNoBody
      | otherwise -> RspBuilder b
    ResponseStream _ _ fb
      | isHead                  -> RspNoBody
      | otherwise               -> RspStream fb th
    ResponseRaw raw _           -> RspRaw raw src (T.tickle th)

data Rsp eff
  = RspNoBody
  | RspFile FilePath (Maybe FilePart) IndexedHeader Boolean (Aff eff Unit)
  | RspBuilder Builder
  | RspStream (StreamingBody eff) (T.Handle eff)
  | RspRaw (Aff eff B.ByteString -> (B.ByteString -> Aff eff Unit) -> Aff eff Unit) (Aff eff B.ByteString) (Aff eff Unit)

sendRsp
  :: forall eff
   . Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> H.HttpVersion
  -> H.Status
  -> H.ResponseHeaders
  -> Rsp (WaiEffects eff)
  -> Aff (WaiEffects eff) (Tuple (Maybe H.Status) (Maybe Int))
sendRsp conn _ ver s hs RspNoBody = do
  _ <- Z.connWriteHead conn s hs
  pure (Tuple (Just s) Nothing)
sendRsp conn _ ver s hs (RspBuilder builder) = do
  _ <- Z.connWriteHead conn s hs
  let
    buffer = Z.connWriteBuffer conn
    size   = Z.connBufferSize conn
  toBufIOWith buffer size (Z.connSendAll conn) builder
  pure (Tuple (Just s) Nothing)
sendRsp conn _ ver s hs (RspStream streamingBody th) = do
  Tuple recv finish <- liftEff $ refinedEff $
                    newByteStringBuilderRecv $ reuseBufferStrategy $
                    pure $ toBuilderBuffer (Z.connWriteBuffer conn) (Z.connBufferSize conn)
  _ <- Z.connWriteHead conn s hs
  let
    send builder = do
      popper <- liftEff $ refinedEff $ recv builder
      let
        go = do
          bs <- liftEff $ refinedEff $ popper
          unless (B.null bs) $ do
            _ <- sendFragment conn th bs
            _ <- nextTick
            go
      go
  _ <- streamingBody send (send flush)
  mbs <- liftEff $ refinedEff $ finish
  _ <- maybe (pure unit) (sendFragment conn th) mbs
  pure (Tuple (Just s) Nothing)
sendRsp conn _ _ _ _ (RspRaw withApp src tickle) = do
  let
    recv = do
      bs <- src
      unless (B.null bs) tickle
      pure bs
    send bs = Z.connSendAll conn bs *> tickle
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
sendRspFile2XX conn ii ver s hs path beg len isHead hook
  | isHead    = sendRsp conn ii ver s hs RspNoBody
  | otherwise = do
      Tuple mfd fresher <- Z.getFd ii path
      let
        fid   = Z.FileId { fileIdPath: path, fileIdFd: mfd }
        hook' = hook *> fresher
      _ <- Z.connWriteHead conn s hs
      _ <- Z.connSendFile conn fid beg len hook'
      pure (Tuple (Just s) (Just len))

sendRspFile404
  :: forall eff
   . Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> H.HttpVersion
  -> H.ResponseHeaders
  -> Aff (WaiEffects eff) (Tuple (Maybe H.Status) (Maybe Int))
sendRspFile404 conn ii ver hs0 = sendRsp conn ii ver s hs (RspBuilder body)
  where
  s    = H.notFound404
  hs   = replaceHeader H.hContentType "text/plain; charset=utf-8" hs0
  body = string7 "File not found"

hasBody :: H.Status -> Boolean
hasBody s = sc /= 204
         && sc /= 304
         && sc >= 200
  where
  sc = H.statusNumber s

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

toBufIOWith
  :: forall eff
   . Z.Buffer
  -> Z.BufSize
  -> (B.ByteString -> Aff eff Unit)
  -> Builder
  -> Aff eff Unit
toBufIOWith buf size io builder = go firstWriter
  where
  firstWriter = runBuilder builder
  runIO len   = bufferIO buf len io
  go writer = do
    Tuple len signal <- liftEff $ refinedEff $ writer buf size
    case signal of
      Done -> runIO len
      More minSize next
        | size < minSize -> throwError (error "toBufIOWith: BufferFull: minSize")
        | otherwise      -> do
            _ <- runIO len
            _ <- delay (wrap 0.00)
            go next
      Chunk bs next -> do
        _ <- runIO len
        _ <- io bs
        _ <- delay (wrap 0.00)
        go next

bufferIO :: forall eff. Z.Buffer -> Int -> (B.ByteString -> Aff eff Unit) -> Aff eff Unit
bufferIO ptr siz io = io $ B.ByteString ptr 0 siz

toBuilderBuffer :: Z.Buffer -> Z.BufSize -> BI.Buffer
toBuilderBuffer ptr size = BI.Buffer ptr (BI.BufferRange ptr (ptr `plusPtr` size))

replaceHeader :: H.HeaderName -> String -> H.ResponseHeaders -> H.ResponseHeaders
replaceHeader k v hdrs = (Tuple k v : Nil) <> deleteBy ((==) `on` fst) (Tuple k v) hdrs
