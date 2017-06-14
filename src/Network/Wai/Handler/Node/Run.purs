module Network.Wai.Handler.Node.Run where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, makeAff)

import Data.ArrayBuffer (allocArrayBuffer)
import Data.ArrayBuffer.TypedArray (newPtr, newUint8Array)
import Data.Bifunctor (lmap)
import Data.ByteString.Node.Stream as BS
import Data.ByteString.Node.File as NF
import Data.Foldable (traverse_)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)

import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.Types (Connection(..), FileId(..), SendFile)
import Network.Wai.Types as H
import Node.HTTP as N
import Node.FS (FileDescriptor)
import Node.FS.Stream (defaultReadStreamOptions, createReadStream, fdCreateReadStream)
import Node.Path (FilePath)


httpConnection :: forall eff. N.Response -> Eff (WaiEffects eff) (Connection (WaiEffects eff))
httpConnection resp = do
  writeBuf <- newPtr <$> (allocArrayBuffer 16384 >>= newUint8Array 0 16384)
  let
    out       = N.responseAsStream resp
    write bs  = BS.write out bs (pure unit) $> unit
    writeHead = liftEff <<< nodeHttpWriteHead resp
  pure $ Connection
    { connSendMany: liftEff <<< traverse_ write
    , connSendAll:  liftEff <<< write
    , connSendFile: sendFileStream out writeHead
    , connWriteHead: writeHead
    , connWriteBuffer: writeBuf
    , connBufferSize: 16384
    }

nodeHttpWriteHead :: forall e. N.Response -> H.ResponseHeaders -> Eff (http :: N.HTTP | e) Unit
nodeHttpWriteHead nresp = traverse_ $ uncurry (N.setHeader nresp) <<< lmap showHeaderName
  where
  showHeaderName (CaseInsensitiveString s) = s

data FileRange = EntireFile | PartOfFile Int Int

sendFileStream
  :: forall r eff
   . BS.Writable r (WaiEffects eff)
  -> (H.ResponseHeaders -> Aff (WaiEffects eff) Unit)
  -> SendFile (WaiEffects eff)
sendFileStream ws sndhdr (FileId { fileIdPath, fileIdFd }) off len act hdr = case fileIdFd of
  Nothing -> sendfileWithHeader ws sndhdr fileIdPath (PartOfFile off len) act hdr
  Just fd -> sendfileFdWithHeader ws sndhdr fd (PartOfFile off len) act hdr

sendfileWithHeader
  :: forall r eff
   . BS.Writable r (WaiEffects eff)
  -> (H.ResponseHeaders -> Aff (WaiEffects eff) Unit)
  -> FilePath
  -> FileRange
  -> Aff (WaiEffects eff) Unit
  -> H.ResponseHeaders
  -> Aff (WaiEffects eff) Unit
sendfileWithHeader ws sndhdr path frange act hdr = do
  _ <- sndhdr hdr
  rs <- liftEff rseff
  _ <- pipeStreamAff rs ws
  act
  where
  rseff = case frange of
    PartOfFile start end ->
      NF.createReadableStreamRangeWith defaultReadStreamOptions start end path
    EntireFile -> createReadStream path

sendfileFdWithHeader
  :: forall r eff
   . BS.Writable r (WaiEffects eff)
  -> (H.ResponseHeaders -> Aff (WaiEffects eff) Unit)
  -> FileDescriptor
  -> FileRange
  -> Aff (WaiEffects eff) Unit
  -> H.ResponseHeaders
  -> Aff (WaiEffects eff) Unit
sendfileFdWithHeader ws sndhdr fd frange act hdr = do
  _ <- sndhdr hdr
  rs <- liftEff $ rseff
  _ <- pipeStreamAff rs ws
  act
  where
  rseff = case frange of
    PartOfFile start end ->
      NF.fdCreateReadableStreamRangeWith defaultReadStreamOptions start end fd
    EntireFile -> fdCreateReadStream fd

-- | use custom pipe, so Node don't `end` Writable stream here.
pipeStreamAff
  :: forall r w eff
   . BS.Readable w eff
  -> BS.Writable r eff
  -> Aff eff Unit
pipeStreamAff r w = makeAff \err suc -> do
  BS.onError r err
  BS.onEnd r do
    suc unit
  pipeNoEnd r w $> unit

foreign import pipeNoEnd
  :: forall r w eff. BS.Readable w eff -> BS.Writable r eff -> Eff eff Unit
