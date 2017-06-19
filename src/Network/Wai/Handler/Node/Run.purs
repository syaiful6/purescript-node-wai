module Network.Wai.Handler.Node.Run where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, makeAff, runAff, forkAff, Canceler, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class (withResource)

import Data.ArrayBuffer (allocArrayBuffer)
import Data.ArrayBuffer.TypedArray (newPtr, newUint8Array)
import Data.Bifunctor (lmap)
import Data.ByteString.Node.Stream as BS
import Data.ByteString.Node.File as NF
import Data.Foldable (traverse_)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List(Nil))
import Data.String as S
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap as SM
import Data.Posix.Signal (Signal(SIGTERM, SIGINT))
import Data.Tuple (Tuple, uncurry)

import Network.Wai (Application, Request(..))
import Network.Wai.Types as H
import Network.Wai.Handler.Node.BodySource (readBody, mkBodySource)
import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.Types as Z
import Network.Wai.Handler.Node.FdCache as F
import Network.Wai.Handler.Node.FileInfoCache as I
import Network.Wai.Handler.Node.Timeout as T

import Node.HTTP as N
import Node.FS (FileDescriptor)
import Node.FS.Stream (defaultReadStreamOptions, createReadStream, fdCreateReadStream)
import Node.Path (FilePath)
import Node.Process as Proc
import Unsafe.Coerce (unsafeCoerce)


httpConnection :: forall eff. N.Response -> Eff (WaiEffects eff) (Z.Connection (WaiEffects eff))
httpConnection resp = do
  writeBuf <- newPtr <$> (allocArrayBuffer 16384 >>= newUint8Array 0 16384)
  let
    out       = N.responseAsStream resp
    write bs  = BS.write out bs (pure unit) $> unit
  pure $ Z.Connection
    { connSendMany: liftEff <<< traverse_ write
    , connSendAll:  liftEff <<< write
    , connSendFile: sendFileStream out
    , connClose: makeAff \_ suc -> BS.end out (suc unit)
    , connWriteHead: \s h -> liftEff $ nodeHttpWriteHead resp s h
    , connWriteBuffer: writeBuf
    , connBufferSize: 16384
    }

recvNodeRequest :: forall eff. N.Request -> Aff (WaiEffects eff) (Request (WaiEffects eff))
recvNodeRequest req = do
  bs <- mkBodySource
  source <- readBody (N.requestAsStream req) bs
  pure $ Request
    { method: httpMethod
    , headers: reqHeaders
    , httpVersion: fromMaybe H.http10 $ H.string2HttpVersion (N.httpVersion req)
    , rawPathInfo: rawPathInfo
    , rawQueryString: fromMaybe "" rawQs
    , query: fromMaybe Nil (H.parseQuery <$> rawQs)
    , pathInfo: pathInfo
    , body: source
    }
  where
  rawPathInfo = N.requestURL req
  idxparam = S.indexOf (S.Pattern "?") rawPathInfo
  rawQs = flip S.drop rawPathInfo <<< (+) 1 <$> idxparam
  pathInfo = H.pathSegments $ fromMaybe rawPathInfo (flip S.take rawPathInfo <$> idxparam)
  reqHeaders :: H.RequestHeaders
  reqHeaders = unsafeCoerce $ (SM.toUnfoldable (N.requestHeaders req) :: List (Tuple String String))
  httpMethod :: H.Method
  httpMethod = fromMaybe H.GET $ H.string2HTTPMethod (N.requestMethod req)

runSettingsServer
  :: forall eff
   . Z.Settings (WaiEffects eff)
  -> Application (WaiEffects eff)
  -> Aff (WaiEffects eff) Unit
runSettingsServer sett@(Z.Settings set) app = withII0 $ runServer sett app
  where
  withII0 action =
    withTimeoutManager \tm ->
      F.withFdCache fdCacheDuration \fdc ->
        I.withFileInfoCache fileInfoCacheDuration \fic ->
          let ii = Z.InternalInfo0 tm fdc fic
          in action ii
  fdCacheDuration = set.fdCacheDuration * 1000.00
  fileInfoCacheDuration = set.fileInfoCacheDuration * 1000.00
  timeoutDuration = set.timeout * 1000.00
  withTimeoutManager f = case set.manager of
    Just tm -> f tm
    Nothing ->
      withResource
        (T.initialize timeoutDuration)
        T.stopManager
        f

runServer
  :: forall eff
   . Z.Settings (WaiEffects eff)
  -> Application (WaiEffects eff)
  -> Z.InternalInfo0 (WaiEffects eff)
  -> Aff (WaiEffects eff) Unit
runServer sett@(Z.Settings set) app ii0 =
  withResource
    (liftEff (N.createServer handleRequest))
    shutdownServer
    (\serv -> connect serv *> trapSignal)
  where
  connect serv = makeAff \_ succ -> case set.socketOption of
    Z.SockTCP hostname port backlog ->
      N.listen serv { hostname, port, backlog } (succ unit)
    Z.SockUnix pathname ->
      N.listenSocket serv pathname (succ unit)

  handleRequest :: N.Request -> N.Response -> Eff (WaiEffects eff) Unit
  handleRequest req res = void $
    runAff
      (set.onException Nothing)
      (const (pure unit))
      (handleRequest' req res)

  handleRequest' :: N.Request -> N.Response -> Aff (WaiEffects eff) Unit
  handleRequest' req res = do
    wreq <- recvNodeRequest req
    void $ forkAff $ withClosedRef \ref ->
      withResource
        (liftEff (httpConnection res))
        (cleanup ref)
        (serve ref wreq)
    where
    serve ref wreq conn = withResource register cancel \th ->
      let ii1 = Z.toInternalInfo th ii0
      in serveConnection wreq conn ii1 sett app
      where
      register = T.register (Z.timeoutManager0 ii0) (cleanup ref conn)
      cancel   = T.tickle

    cleanup ref conn = do
      isClosed <- liftEff $ Ref.modifyRef' ref (\x -> { state: true, value: x })
      unless isClosed $ Z.connClose conn

    withClosedRef inner = liftEff (Ref.newRef false) >>= inner

serveConnection
  :: forall eff
   . Request (WaiEffects eff)
  -> Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> Z.Settings (WaiEffects eff)
  -> Application (WaiEffects eff)
  -> Aff (WaiEffects eff) Unit
serveConnection req conn ii set app = pure unit

nodeHttpWriteHead :: forall e. N.Response -> H.Status -> H.ResponseHeaders -> Eff (http :: N.HTTP | e) Unit
nodeHttpWriteHead nresp (H.Status co reas) hdrs = do
  _ <- traverse_ (uncurry (N.setHeader nresp) <<< lmap showHeaderName) hdrs
  _ <- N.setStatusCode nresp co
  N.setStatusMessage nresp reas
  where
  showHeaderName (CaseInsensitiveString s) = s

data FileRange = EntireFile | PartOfFile Int Int

sendFileStream
  :: forall r eff
   . BS.Writable r (WaiEffects eff)
  -> Z.SendFile (WaiEffects eff)
sendFileStream ws (Z.FileId { fileIdPath, fileIdFd }) off len act = case fileIdFd of
  Nothing -> sendfilePath ws fileIdPath (PartOfFile off len) act
  Just fd -> sendfileFd ws fd (PartOfFile off len) act

sendfilePath
  :: forall r eff
   . BS.Writable r (WaiEffects eff)
  -> FilePath
  -> FileRange
  -> Aff (WaiEffects eff) Unit
  -> Aff (WaiEffects eff) Unit
sendfilePath ws path frange act = do
  rs <- liftEff rseff
  _ <- pipeStreamAff rs ws
  act
  where
  rseff = case frange of
    PartOfFile start end ->
      NF.createReadableStreamRangeWith defaultReadStreamOptions start end path
    EntireFile -> createReadStream path

sendfileFd
  :: forall r eff
   . BS.Writable r (WaiEffects eff)
  -> FileDescriptor
  -> FileRange
  -> Aff (WaiEffects eff) Unit
  -> Aff (WaiEffects eff) Unit
sendfileFd ws fd frange act = do
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
  runFn2 pipeNoEnd r w $> unit

shutdownServer :: forall eff. N.Server -> Aff (WaiEffects eff) Unit
shutdownServer serv = do
  _ <- runFn2 closeHttpServer nonCanceler serv
  liftEff $ Proc.exit 0

trapSignal :: forall eff. Aff (WaiEffects eff) Unit
trapSignal = sigterm <|> sigint
  where
  sigterm = makeAff \_ succ -> Proc.onSignal SIGTERM (succ unit)
  sigint  = makeAff \_ succ -> Proc.onSignal SIGINT (succ unit)

foreign import pipeNoEnd
  :: forall r w eff. Fn2 (BS.Readable w eff) (BS.Writable r eff) (Eff eff Unit)

foreign import closeHttpServer
  :: forall eff. Fn2 (Canceler eff) N.Server (Aff (http :: N.HTTP | eff) Unit)
